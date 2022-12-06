import Data.List (nub)
import Debug.Trace

main :: IO ()
main = do
    content <- readFile "./input.txt"
    print $ packetStartCount content
    print $ messageStartCount content

messageStartCount packet | length packet >= 14 = messageStartCountInner packet 14
messageStartCount _ = error "Packet not long enough to calculate start of message marker"

messageStartCountInner packet count | listUnique (take 14 packet) = count
messageStartCountInner packet count = messageStartCountInner (tail packet) (count + 1)

packetStartCount packet | length packet >= 4 = packetStartCountInner packet 4
packetStartCount _ = error "Packet not long enough to calculate start of packet marker"

packetStartCountInner (a:b:c:d:xs) count | listUnique [a, b, c, d] = count
packetStartCountInner (a:b:c:d:xs) count = packetStartCountInner (b:c:d:xs) (count + 1)

listUnique list = length list == length (nub list)