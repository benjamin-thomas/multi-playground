f :: [Int] -> [Int]
f lst =
    snd
        <$> filter
            (even . fst)
            (zip [1 ..] lst)

-- This part deals with the Input and Output and can be used as it is. Do not modify it.
main = do
    inp <- getContents
    mapM_ print . f . map read . lines $ inp