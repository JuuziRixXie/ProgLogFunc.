cesar.hs

import Data.Char

-- Función para cifrar un caracter usando el cifrado César
cipherChar :: Int -> Char -> Char
cipherChar shift char
  | isLower char = shiftChar' 'a' shift char
  | isUpper char = shiftChar' 'A' shift char
  | otherwise    = char

-- Función auxiliar para desplazar un caracter en el alfabeto
shiftChar' :: Char -> Int -> Char -> Char
shiftChar' base shift char = chr $ (ord char - ord base + shift) mod 26 + ord base

-- Función para cifrar una cadena usando el cifrado César
caesarCipher :: Int -> String -> String
caesarCipher shift = map (cipherChar shift)

-- Función principal
main :: IO ()
main = do
  putStrLn "Introduce el texto a cifrar:"
  inputText <- getLine

  putStrLn "Introduce el desplazamiento:"
  inputShift <- getLine
  let desplazamiento = read inputShift :: Int

  let mensajeCifrado = caesarCipher desplazamiento inputText

  putStrLn $ "Mensaje Original: " ++ inputText
  putStrLn $ "Mensaje Cifrado: " ++ mensajeCifrado
