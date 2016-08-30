{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE RecordWildCards  #-}


module Main where


import CliArgs


main :: IO ()
main = runWithArgs $ \args@CliArgs{..} ->
  putStrLn $ show args
