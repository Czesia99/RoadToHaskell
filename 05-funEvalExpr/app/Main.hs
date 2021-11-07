module Main where

import Lib
import Eval
import Text.Printf
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> printUsage >> invalidArgument
        [str] -> runEvalExpr str
        _ -> printUsage  >> invalidArgument
        where
            runEvalExpr str = case evalExpr str of
                Just x -> printf "%.2f\n" x
                Nothing -> printErrAndReturn "can't compute" 84
            invalidArgument = printErrAndReturn "Invalid argument" 84

--                         . . . .
--                         ,`,`,`,`,
--   . . . .               `\`\`\`\;
--   `\`\`\`\`,            ~|;!;!;\!
--    ~\;\;\;\|\          (--,!!!~`!       .
--   (--,\\\===~\         (--,|||~`!     ./
--    (--,\\\===~\         `,-,~,=,:. _,//
--     (--,\\\==~`\        ~-=~-.---|\;/J,
--      (--,\\\((```==.    ~'`~/       a |
--        (-,.\\('('(`\\.  ~'=~|     \_.  \
--           (,--(,(,(,'\\. ~'=|       \\_;)
--             (,-( ,(,(,;\\ ~=/        \
--             (,-/ (.(.(,;\\,/          )
--              (,--/,;,;,;,\\         ./------.
--                (==,-;-'`;'         /_,----`. \
--        ,.--_,__.-'                    `--.  ` \
--       (='~-_,--/        ,       ,!,___--. \  \_)
--      (-/~(     |         \   ,_-         | ) /_|
--      (~/((\    )\._,      |-'         _,/ /
--       \\))))  /   ./~.    |           \_\;
--    ,__/////  /   /    )  /
--     '===~'   |  |    (, (.
--              / /       \. \
--            _/ /          \_\
--           /_!/            )_\