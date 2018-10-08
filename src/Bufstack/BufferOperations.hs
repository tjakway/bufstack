{-# LANGUAGE ScopedTypeVariables #-}
module Bufstack.BufferOperations where

import Neovim
import Neovim.API.String
import Bufstack.Core
import Bufstack.Util
import Bufstack.Error
import Bufstack.Class.HasNumber

import Control.Monad (mapM, foldM)
import Data.List (nub, sortBy, findIndex)
import Data.Function (on)

pushBuffer :: Buffer -> BufstackM ()
pushBuffer buf = modifyBuffersM_ (buf :)

popBuffer :: BufstackM (Maybe Buffer)
popBuffer = modifyBuffersMs tailMaybeS 
    where tailMaybeS [] = (Nothing, [])
          tailMaybeS (x:xs) = (Just x, xs)

removeBuffer :: Buffer -> BufstackM ()
removeBuffer b = modifyBuffersM_ $ filter (/= b)

-- | look at the top buffer without removing any
peekBuffer :: BufstackM (Maybe Buffer)
peekBuffer = getBuffers >>= return . headMaybe
    where headMaybe [] = Nothing
          headMaybe (x:_) = Just x

replaceBufstack :: [Buffer] -> BufstackM ()
replaceBufstack newBuffers = modifyBuffersM_ (const newBuffers)

reverseBufstack :: BufstackM ()
reverseBufstack = modifyBuffersM_ reverse

nextBufE :: BufstackMEither ()
nextBufE = do
        let accEithers acc (buf, getNumF) = do
                        x <- getNumF
                        return $ case (acc, x) of (Left errs, Left e) -> Left (e : errs)
                                                  (Left errs, _) -> Left errs
                                                  (Right ys, Right y) -> Right ((buf, y) : ys)
                                                  (_, Left e) -> Left [e]

            joinErrors x = let e = pretty "Accumulated errors from getNumber applied to results from nvim_list_bufs: "
                               in ErrorMessage (e <> (pretty . show . reverse $ x))
            mapLeft f (Left x) = Left (f x)
            mapLeft _ (Right y) = Right y

        numberedBufs <- nvim_list_bufs `bindNvimEither` 
                            (fmap (mapLeft joinErrors) . 
                             foldM accEithers (Right []) . 
                             map (\x -> (x, getNumber x)) . 
                             nub)

        currentBuf <- nvim_get_current_buf
        currentBufNum <-   (return currentBuf) `bindNvimEither` getNumber

        let headMaybe [] = Nothing
            headMaybe (x:_) = Just x

            atMaybe xs i
                    | (length xs) > (i + 1) = Nothing
                    | otherwise = Just (xs !! i)

            f :: Either NeovimException (Maybe Buffer)
            f = do -- Either monad
               sortedNumberedBufs <- sortBy (compare `on` snd) <$> numberedBufs
               currentBuf' <- currentBuf
               currentBufNum' <- currentBufNum

               currentBufIndex <- maybe (Left . ErrorMessage . pretty $ "Could not find current buf index") return .
                                       findIndex ((== currentBufNum') . snd) $ sortedNumberedBufs

               let nextBufIndex = currentBufIndex + 1

                   -- make sure we don't try to switch to the current buf
                   filterCurrentBuf (Just x) = if x == currentBuf' then Nothing
                                                                   else Just x
                   filterCurrentBuf Nothing = Nothing


               return . filterCurrentBuf . fmap fst $ 
                    if nextBufIndex >= (length sortedNumberedBufs)
                        -- should never return Nothing, but just in case
                        then headMaybe sortedNumberedBufs 
                        else atMaybe sortedNumberedBufs nextBufIndex 

        case f of Right (Nothing) -> return . return $ () -- there's only 1 buffer, do nothing
                  Right (Just nextBuf) -> pushBuffer nextBuf >> nvim_set_current_buf nextBuf
                  Left x -> return . Left $ x


nextBufFunction :: BufstackM ()
nextBufFunction = nextBufE >>= handleError

-- | TODO: use the previous buffer (numerically) if the stack is empty
-- (i.e. if popBuffer returns Nothing)
previousBufFunction :: BufstackM ()
previousBufFunction = popBuffer >>= f
    where f (Just buf) = nvim_set_current_buf' buf
          f _ = return ()
