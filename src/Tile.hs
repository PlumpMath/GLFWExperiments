module Tile where

import qualified Data.Map					as Map


data Floor =
		BasicFLoor Integer Integer
	|	CyberFloor Integer Integer
	deriving (Show,Eq,Ord)

data Tile =  BasicHallway Integer Integer | Floor Integer Integer