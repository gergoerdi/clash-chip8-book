{-# LANGUAGE RecordWildCards, NumericUnderscores, ApplicativeDo #-}
module CHIP8.Video where

import Clash.Prelude
import RetroClash.VGA
import RetroClash.Video
import RetroClash.Utils

import Data.Maybe
import Control.Arrow (first)
import CHIP8.Types
import Control.Monad (join)

-- | 25 MHz clock, needed for the VGA mode we use.
createDomain vSystem{vName="Dom25", vPeriod = hzToPeriod 25_175_000}

video
    :: (HiddenClockResetEnable Dom25)
    => Signal Dom25 (Maybe (VidY, VidRow))
    -> (Signal Dom25 Bool, VGAOut Dom25 8 8 8)
video write = (frameEnd, vgaOut vgaSync rgb)
  where
    VGADriver{..} = vgaDriver vga640x480at60
    frameEnd = isFalling False (isJust <$> vgaY)

    vgaX' = scale @64 (SNat @10) vgaX
    vgaY' = scale @32 (SNat @10) . center $ vgaY

    rgb = maybe border palette <$> pixel

    border = (0x30, 0x30, 0x50)

    palette 0 = (0x00, 0x00, 0x00)
    palette 1 = (0xff, 0xcc, 0x33)

    lineStart = isRising False $ (isJust <$> vgaX')
    newY = changed Nothing vgaY'
    newX = changed Nothing vgaX'
    visible = isJust <$> vgaX' .&&. isJust <$> vgaY'

    addr = mux lineStart vgaY' (pure Nothing)
    write' = fmap (first bitCoerce) <$> write
    load = blockRam1 ClearOnReset (SNat @32) 0x00 (fromMaybe 0 <$> addr) write'

    row = register 0 $
        mux (register False $ isJust <$> addr) load $
        mux (not <$> lineStart .&&. newX) ((`shiftL` 1) <$> row) $
        row
    pixel = enable visible $ msb <$> row
