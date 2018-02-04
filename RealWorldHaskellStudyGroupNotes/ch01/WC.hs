-- count chars
-- main = interact wordCount
--           where wordCount input = show (length input)

-- count words
 main = interact wordCount
     where wordCount input = show (length (words input))

--  original
-- main = interact wordCount
--  where wordCount input = show (length (lines input)) ++ "\n"
