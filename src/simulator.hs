{-# LANGUAGE OverloadedStrings, RecordWildCards, NumericUnderscores #-}
module Main where

import Clash.Prelude hiding ((!))

import CHIP8.Types
import CHIP8.CPU
import CHIP8.Font
import CHIP8.Input

import RetroClash.Sim.SDL
import RetroClash.Barbies
import RetroClash.Keypad
import Control.Monad.State
import Data.Maybe
import Data.Word
import Data.Array ((!))
import Data.Array.IO
import qualified Data.ByteString as BS
import Data.Foldable (traverse_)
import Data.Bits
import Debug.Trace
import SDL.Event as SDL
import SDL.Input.Keyboard
import SDL.Input.Keyboard.Codes

world
    :: IOUArray Word16 Word8
    -> IOUArray Word8 Word64
    -> Vec (4 * 4) Bool
    -> [(Bool, Key)]
    -> Bool
    -> Pure CPUOut
    -> IO (Pure CPUIn)
world ram vid keyState keyEvent firstForFrame CPUOut{..} = do
    memRead <- readMem _memAddr
    vidRead <- readVid _vidAddr
    tick <- return firstForFrame
    keyEvent <- return $ listToMaybe keyEvent
    keyState <- return keyState

    traverse_ (writeMem _memAddr) _memWrite
    traverse_ (writeVid _vidAddr) _vidWrite

    return CPUIn{..}
  where
    readMem addr = readArray ram (fromIntegral addr)
    readVid addr = readArray vid (fromIntegral addr)

    writeMem addr = writeArray ram (fromIntegral addr)
    writeVid addr = writeArray vid (fromIntegral addr)

scanLayout :: Matrix 4 4 Scancode
scanLayout =
    (Scancode1 :> Scancode2 :> Scancode3 :> Scancode4 :> Nil) :>
    (ScancodeQ :> ScancodeW :> ScancodeE :> ScancodeR :> Nil) :>
    (ScancodeA :> ScancodeS :> ScancodeD :> ScancodeF :> Nil) :>
    (ScancodeZ :> ScancodeX :> ScancodeC :> ScancodeV :> Nil) :>
    Nil

scanMap :: Vec 16 Scancode
scanMap = scatter (repeat ScancodeUnknown) (concat layout) (concat scanLayout)

main :: IO ()
main = do
    ram <- do
        ram <- newArray (0, 4095) 0
        zipWithM_ (writeArray ram) [0x000..] (toList hexDigits)

        img <- BS.readFile "roms/hidden.ch8"
        zipWithM_ (writeArray ram) [0x200..] (BS.unpack img)
        return ram
    vid <- newArray (0, 31) 0

    let initInput = CPUIn
            { memRead = 0
            , vidRead = 0
            , tick = False
            }
    flip evalStateT (initInput, initState) $
      withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        let keyState = fmap keyDown scanMap
            keyEvents =
                [ (keyboardEventKeyMotion == SDL.Pressed, key)
                | KeyboardEvent KeyboardEventData{..} <- fmap eventPayload events
                , let scanCode = keysymScancode keyboardEventKeysym
                , Just key <- pure $ elemIndex scanCode scanMap
                ]

        let sim firstForFrame = do
                (inp, s) <- get
                let (out, s') = runState (cpuMachine inp) s
                -- TODO: feed events once every 100 cycles or somesuch
                inp' <- liftIO $ world ram vid keyState (if firstForFrame then keyEvents else []) firstForFrame out
                put (inp', s')

        sim True
        replicateM_ 10_000 $ sim False

        vidArr <- liftIO $ freeze vid
        return $ rasterizePattern @64 @32 $ \x y ->
          let fg = (0xe7, 0xc2, 0x51)
              bg = (0x20, 0x20, 0x20)
              row = vidArr ! fromIntegral y
          in if testBit row (fromIntegral (maxBound - x)) then fg else bg
  where
    videoParams = MkVideoParams
        { windowTitle = "CHIP-8"
        , screenScale = 20
        , screenRefreshRate = 60
        , reportFPS = True
        }
