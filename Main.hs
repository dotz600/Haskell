import System.IO
import Data.List (isPrefixOf)
import System.Directory (getDirectoryContents)
import System.FilePath
import Control.Monad (mapM, foldM)

add :: String
add = ("@SP\n" ++ "A=M-1\n" ++ "D=M\n" ++ "A=A-1\n" ++ "M=M+D\n" ++ "@SP\n" ++ "M=M-1\n")

sub :: String
sub = ("@SP\n" ++ "A=M-1\n" ++ "D=M\n" ++ "A=A-1\n" ++ "M=M-D\n" ++ "@SP\n" ++ "M=M-1\n")

neg :: String
neg = ("@0\n" ++ "D=A\n" ++ "@SP\n" ++ "A=M-1\n" ++ "M=D-M\n")

myNot :: String
myNot = "@SP\n" ++ "A=M-1\n" ++ "M=!M\n"

myOr :: String
myOr = "@SP\n" ++ "A=M-1\n" ++ "D=M\n" ++ "A=A-1\n" ++ "M=D|M\n" ++ "@SP\n" ++ "M=M-1\n"

myAnd :: String
myAnd = "@SP\n" ++ "A=M-1\n" ++ "D=M\n" ++ "A=A-1\n" ++ "M=D&M\n" ++ "@SP\n" ++ "M=M-1\n"

eq :: Int -> String
eq lableNum = 
    "@SP\n" ++ "A=M-1\n" ++ "D=M\n" ++ "A=A-1\n" ++ "D=M-D\n" ++ "@SP\n"++ "M=M-1\n"
    ++ "@TRUE" ++ show(lableNum) ++ "\nD;JEQ\n" ++ "@SP\n" ++ "A=M-1\n" ++ "M=0\n"
    ++ "@END" ++ show(lableNum) ++ "\n0;JMP\n" ++ "(TRUE" ++ show(lableNum) ++ ")\n" ++ "@SP\n" ++ "A=M-1\n" ++ "M=-1\n" 
    ++ "(END" ++ show(lableNum) ++ ")\n"

lt :: Int -> String
lt lableNum =
    "@SP\n" ++ "A=M-1\n" ++ "D=M\n" ++ "A=A-1\n" ++ "D=M-D\n" ++ "@SP\n"++ "M=M-1\n"
    ++ "@TRUE" ++ show(lableNum) ++ "\nD;JLT\n" ++ "@SP\n" ++ "A=M-1\n" ++ "M=0\n"
    ++ "@END" ++ show(lableNum) ++ "\n0;JMP\n" ++ "(TRUE" ++ show(lableNum) ++ ")\n" ++ "@SP\n" ++ "A=M-1\n" ++ "M=-1\n" 
    ++ "(END" ++ show(lableNum) ++ ")\n"

gt :: Int -> String
gt lableNum = 
    "@SP\n" ++ "A=M-1\n" ++ "D=M\n" ++ "A=A-1\n" ++ "D=M-D\n" ++ "@SP\n"++ "M=M-1\n"
    ++ "@TRUE" ++ show(lableNum) ++ "\nD;JGT\n" ++ "@SP\n" ++ "A=M-1\n" ++ "M=0\n"
    ++ "@END" ++ show(lableNum) ++ "\n0;JMP\n" ++ "(TRUE" ++ show(lableNum) ++ ")\n" ++ "@SP\n" ++ "A=M-1\n" ++ "M=-1\n" 
    ++ "(END" ++ show(lableNum) ++ ")\n"

label :: String -> String
label _label = "(" ++ _label ++ ")\n"

goto :: String -> String
goto label = "@" ++ label ++ "\n0;JMP\n"

if_goto :: String -> String
if_goto label = "@SP\n" ++ "M=M-1\n" ++ "A=M\n" ++ "D=M\n" ++ "@" ++ label ++ "\nD;JNE\n" --double check maby need JEQ

pushFromSegment :: String -> String -> String
pushFromSegment segment i =
    let num = read i :: Int
    in "@" ++ segment ++ "\nA=M\n" ++ concat (replicate num "A=A+1\n") ++ "D=M\n" ++ general_push

pushPointer :: String -> String
pushPointer param =
    case param of
        "0" -> "@THIS\n" ++ "D=M\n" ++ general_push
        "1" -> "@THAT\n" ++ "D=M\n" ++ general_push

pushConstant :: String -> String
pushConstant param = "@" ++ param ++ "\n" ++ "D=A\n" ++ general_push

pushTemp :: String -> String
pushTemp param = "@" ++ show(5 + (read param :: Int)) ++ "\nD=M\n" ++ general_push

pushStatic :: String -> String -> String
pushStatic filename param = "@" ++ filename ++ "." ++ param ++ "\nD=M\n" ++ general_push

general_push :: String
general_push = "@SP\n" ++ "A=M\n" ++ "M=D\n" ++ "@SP\n" ++ "M=M+1\n"

push :: String -> [String] -> String
push fileName op = do
    let segment = head op
    let parameter = (op !! 1)
    case segment of
        "constant" -> pushConstant parameter
        "local"    -> pushFromSegment "LCL" parameter
        "argument" -> pushFromSegment "ARG" parameter
        "this"     -> pushFromSegment "THIS" parameter
        "that"     -> pushFromSegment "THAT" parameter
        "temp"     -> pushTemp parameter
        "pointer"  -> pushPointer parameter
        "static"   -> pushStatic fileName parameter

popToSegment :: String -> String -> String
popToSegment segment i =
    let num = read i :: Int
    in general_pop ++ "@" ++ segment ++ "\nA=M\n" ++ concat (replicate num "A=A+1\n") ++ "M=D\n"

popPointer :: String -> String
popPointer param =
    case param of
        "0" -> general_pop ++ "@THIS\n" ++ "M=D\n"
        "1" -> general_pop ++ "@THAT\n" ++ "M=D\n"

popTemp :: String -> String
popTemp param = general_pop ++ "@" ++ show(5 + (read param :: Int)) ++ "\nM=D\n"

popStatic :: String -> String -> String
popStatic filename param = general_pop ++ "@" ++ filename ++ "." ++ param ++ "\nM=D\n"

general_pop :: String
general_pop = "@SP\n" ++ "A=M-1\n" ++ "D=M\n" ++ "@SP\n" ++ "M=M-1\n"

pop :: String -> [String] -> String
pop fileName op = do
    let segment = head op
    let parameter = (op !! 1)
    case segment of
        "local"    -> popToSegment "LCL" parameter
        "argument" -> popToSegment "ARG" parameter
        "this"     -> popToSegment "THIS" parameter
        "that"     -> popToSegment "THAT" parameter
        "temp"     -> popTemp parameter
        "pointer"  -> popPointer parameter
        "static"   -> popStatic fileName parameter


operation :: String -> Int -> String -> (String, Int)
operation filename labelCount oper =
    if null oper
    then ("", labelCount)
    else 
        let op = words oper
            (cmd, params) = (head op, tail op)
        in case cmd of
            "add"  -> (add, labelCount)
            "sub"  -> (sub, labelCount)
            "neg"  -> (neg, labelCount)
            "push" -> (push filename params, labelCount)
            "pop"  -> (pop filename params, labelCount)
            "not"  -> (myNot, labelCount)
            "and"  -> (myAnd, labelCount)
            "or"   -> (myOr, labelCount)
            "eq"   -> (eq labelCount, labelCount + 1)
            "lt"   -> (lt labelCount, labelCount + 1)
            "gt"   -> (gt labelCount, labelCount + 1)
            "label" -> (label (head params), labelCount)
            "goto" -> (goto (head params), labelCount)
            "if-goto"  -> (if_goto (head params), labelCount)
            _      -> ("", labelCount)


main :: IO ()
main = do
    putStrLn "Enter the path of the directory:"
    file <- getLine
    contents <- readFile file
    let linesOfFile = lines contents
    let filename = takeBaseName file
    let lableCount = 0
    let initialLabelCount = 0
    (output, _) <- foldM (\(acc, lableCount) line ->
                            let (instr, newLableCount) = operation filename lableCount line
                            in return (acc ++ instr ++ "\n", newLableCount)) ("", 0) linesOfFile
    let outputFilePath = (takeDirectory file) </> (takeBaseName file ++ ".asm")
    writeFile outputFilePath output