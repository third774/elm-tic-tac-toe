module IndexedFoldl exposing (indexedFoldl)


indexedFoldl : (Int -> a -> b -> b) -> b -> List a -> b
indexedFoldl func initialValue list =
    indexedFoldlInternal 0 func initialValue list


indexedFoldlInternal : Int -> (Int -> a -> b -> b) -> b -> List a -> b
indexedFoldlInternal currentIndex func accumulator list =
    case list of
        [] ->
            accumulator

        head :: tail ->
            indexedFoldlInternal (currentIndex + 1) func (func currentIndex head accumulator) tail
