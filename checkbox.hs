#!/usr/bin/env runhaskell
-- convert "[ ]" and "[x]" at the start of a list item to unicode ☐ (U+2610) and ☑ (U+2611)
-- i.e. crude implementation of github's markdown lists
-- later: could try format-specific (actual checkbox for HTML; latex for latex)

import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter checkbox
      where checkbox (BulletList listOfBlocks) = BulletList (map toCheckbox listOfBlocks)
            checkbox x = x


-- helper function: ctor is Plain or Para. We construct [Plain/Para [str, restplain], rest]
-- where we pass arguments in like::
-- toCheckbox (Plain x:rest) = helper2 Plain x rest (so x is the first elt of the Para/Plain)
helper :: ([Inline] -> Block) -> [Inline] -> [Block] -> [Block]
helper ctor (Str "[x]" : restplain) rest = (ctor (Str "\x2611" : restplain)) : rest
helper ctor (Str "[" : Space : Str "]" : restplain) rest = (ctor (Str "\x2610" : restplain)) : rest
helper ctor x rest = ctor x : rest

-- can't work out how to abstract away the Plain/Para.
-- helper function helps a bit.
toCheckbox :: [Block] -> [Block]
toCheckbox (Plain x : rest) = helper (Plain) (x) (rest)
toCheckbox (Para x : rest) = helper (Para) (x) (rest)
toCheckbox x = x

-- alternative (perhaps a bit more explicit)
-- helper function: ctor is Plain or Para and we construct [Plain/Para [str, restplain], rest]
-- helper ctor restplain rest str = ctor ((Str str):restplain) : rest
-- toCheckbox :: [Block] -> [Block]
-- toCheckbox ((Plain (Str "[x]":restplain)):rest) = helper Plain restplain rest "\x2611"
-- toCheckbox ((Para (Str "[x]":restplain)):rest) = helper Para restplain rest "\x2611"
-- toCheckbox ((Plain (Str "[":Space:Str"]":restplain)):rest) = helper Plain restplain rest "\x2610"
-- toCheckbox ((Para (Str "[":Space:Str"]":restplain)):rest) = helper Para restplain rest "\x2610"
-- toCheckbox x = x
