import Opcodes (Instruction (..), decode, encode)
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.QuickCheck
    ( Gen
    , Testable (property)
    , choose
    , forAll
    , oneof
    )
import Types (Address (..), Byte (..), Height (..), Nibble (..))

main :: IO ()
main = hspec $ do
    describe "read" $ do
        it "is inverse to show"
            $ property
            $ \x -> (read . show) x `shouldBe` (x :: Int)
    describe "decode" $ do
        it "is inverse to encode"
            $ property
            $ forAll genInstruction
            $ \x -> (decode . encode) x `shouldBe` (x :: Instruction)

genInstruction :: Gen Instruction
genInstruction =
    oneof
        [ pure ClearScreen
        , Jump <$> genAddress
        , SetRegister <$> genNibble <*> genByte
        , AddToRegister <$> genNibble <*> genByte
        , SetIndexRegister <$> genAddress
        , Display <$> genNibble <*> genNibble <*> genHeight
        , pure End
        ]

genHeight :: Gen Height
genHeight = Height <$> choose (0, 15)

genByte :: Gen Byte
genByte = Byte <$> choose (0, 255)

genNibble :: Gen Nibble
genNibble = Nibble <$> choose (0, 15)

genAddress :: Gen Address
genAddress = Address <$> choose (0, 4095)
