module Spec_Day4 where

import Day4
import Test.HUnit

testInputString =
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\
  \\n\
  \22 13 17 11  0\n\
  \ 8  2 23  4 24\n\
  \21  9 14 16  7\n\
  \ 6 10  3 18  5\n\
  \ 1 12 20 15 19\n\
  \\n\
  \ 3 15  0  2 22\n\
  \ 9 18 13 17  5\n\
  \19  8  7 25 23\n\
  \20 11 10 24  4\n\
  \14 21 16 12  6\n\
  \\n\
  \14 21 17 24  4\n\
  \10 16 15  9 19\n\
  \18  8 23 26 20\n\
  \22 11 13  6  5\n\
  \ 2  0 12  3  7"

testInput = [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]
testCell =
  Cell
    { value = 22,
      marked = False
    }

testRow =
  [ Cell {value = 22, marked = False},
    Cell {value = 13, marked = False},
    Cell {value = 17, marked = False},
    Cell {value = 11, marked = False},
    Cell {value = 0, marked = False}
  ]

testBingoBoard1 =
  Board
    { hasWon = False,
      rows =
        [ [ Cell {value = 22, marked = False},
            Cell {value = 13, marked = False},
            Cell {value = 17, marked = False},
            Cell {value = 11, marked = False},
            Cell {value = 0, marked = False}
          ],
          [ Cell {value = 8, marked = False},
            Cell {value = 2, marked = False},
            Cell {value = 23, marked = False},
            Cell {value = 4, marked = False},
            Cell {value = 24, marked = False}
          ],
          [ Cell {value = 21, marked = False},
            Cell {value = 9, marked = False},
            Cell {value = 14, marked = False},
            Cell {value = 16, marked = False},
            Cell {value = 7, marked = False}
          ],
          [ Cell {value = 6, marked = False},
            Cell {value = 10, marked = False},
            Cell {value = 3, marked = False},
            Cell {value = 18, marked = False},
            Cell {value = 5, marked = False}
          ],
          [ Cell {value = 1, marked = False},
            Cell {value = 12, marked = False},
            Cell {value = 20, marked = False},
            Cell {value = 15, marked = False},
            Cell {value = 19, marked = False}
          ]
        ]
    }

testBingoBoard2 =
  Board
    { hasWon = False,
      rows =
        [ [ Cell {value = 3, marked = False},
            Cell {value = 15, marked = False},
            Cell {value = 0, marked = False},
            Cell {value = 2, marked = False},
            Cell {value = 22, marked = False}
          ],
          [ Cell {value = 9, marked = False},
            Cell {value = 18, marked = False},
            Cell {value = 13, marked = False},
            Cell {value = 17, marked = False},
            Cell {value = 5, marked = False}
          ],
          [ Cell {value = 19, marked = False},
            Cell {value = 8, marked = False},
            Cell {value = 7, marked = False},
            Cell {value = 25, marked = False},
            Cell {value = 23, marked = False}
          ],
          [ Cell {value = 20, marked = False},
            Cell {value = 11, marked = False},
            Cell {value = 10, marked = False},
            Cell {value = 24, marked = False},
            Cell {value = 4, marked = False}
          ],
          [ Cell {value = 14, marked = False},
            Cell {value = 21, marked = False},
            Cell {value = 16, marked = False},
            Cell {value = 12, marked = False},
            Cell {value = 6, marked = False}
          ]
        ]
    }

testBingoBoard3 =
  Board
    { hasWon = False,
      rows =
        [ [ Cell {value = 14, marked = False},
            Cell {value = 21, marked = False},
            Cell {value = 17, marked = False},
            Cell {value = 24, marked = False},
            Cell {value = 4, marked = False}
          ],
          [ Cell {value = 10, marked = False},
            Cell {value = 16, marked = False},
            Cell {value = 15, marked = False},
            Cell {value = 9, marked = False},
            Cell {value = 19, marked = False}
          ],
          [ Cell {value = 18, marked = False},
            Cell {value = 8, marked = False},
            Cell {value = 23, marked = False},
            Cell {value = 26, marked = False},
            Cell {value = 20, marked = False}
          ],
          [ Cell {value = 22, marked = False},
            Cell {value = 11, marked = False},
            Cell {value = 13, marked = False},
            Cell {value = 6, marked = False},
            Cell {value = 5, marked = False}
          ],
          [ Cell {value = 2, marked = False},
            Cell {value = 0, marked = False},
            Cell {value = 12, marked = False},
            Cell {value = 3, marked = False},
            Cell {value = 7, marked = False}
          ]
        ]
    }
testBingoBoard3WinState =
  Board
    { hasWon = True,
      rows =
        [ [ Cell {value = 14, marked = True},
            Cell {value = 21, marked = True},
            Cell {value = 17, marked = True},
            Cell {value = 24, marked = True},
            Cell {value = 4, marked = True}
          ],
          [ Cell {value = 10, marked = False},
            Cell {value = 16, marked = False},
            Cell {value = 15, marked = False},
            Cell {value = 9, marked = True},
            Cell {value = 19, marked = False}
          ],
          [ Cell {value = 18, marked = False},
            Cell {value = 8, marked = False},
            Cell {value = 23, marked = True},
            Cell {value = 26, marked = False},
            Cell {value = 20, marked = False}
          ],
          [ Cell {value = 22, marked = False},
            Cell {value = 11, marked = True},
            Cell {value = 13, marked = False},
            Cell {value = 6, marked = False},
            Cell {value = 5, marked = True }
          ],
          [ Cell {value = 2, marked = True},
            Cell {value = 0, marked = True},
            Cell {value = 12, marked = False},
            Cell {value = 3, marked = False},
            Cell {value = 7, marked = True}
          ]
        ]
    }

testBingoBoardWithInput7 =
  Board
    { hasWon = False,
      rows =
        [ [ Cell {value = 22, marked = False},
            Cell {value = 13, marked = False},
            Cell {value = 17, marked = False},
            Cell {value = 11, marked = False},
            Cell {value = 0, marked = False}
          ],
          [ Cell {value = 8, marked = False},
            Cell {value = 2, marked = False},
            Cell {value = 23, marked = False},
            Cell {value = 4, marked = False},
            Cell {value = 24, marked = False}
          ],
          [ Cell {value = 21, marked = False},
            Cell {value = 9, marked = False},
            Cell {value = 14, marked = False},
            Cell {value = 16, marked = False},
            Cell {value = 7, marked = True}
          ],
          [ Cell {value = 6, marked = False},
            Cell {value = 10, marked = False},
            Cell {value = 3, marked = False},
            Cell {value = 18, marked = False},
            Cell {value = 5, marked = False}
          ],
          [ Cell {value = 1, marked = False},
            Cell {value = 12, marked = False},
            Cell {value = 20, marked = False},
            Cell {value = 15, marked = False},
            Cell {value = 19, marked = False}
          ]
        ]
    }

testParseStringToCell = TestCase (assertEqual "string integer should convert to cell correctly" testCell $ parseStringToCell "22")

testParseStringToRow = TestCase (assertEqual "space seperated integers should convert to a row" testRow $ parseStringToRow "22 13 17 11 0")

testParseStringToGame = TestCase (assertEqual "should parse the entire game correctly" (testInput, [testBingoBoard1, testBingoBoard2, testBingoBoard3]) $ parseStringToGame testInputString)

testPlayBingoStep = TestCase (assertEqual "playBingo" testBingoBoardWithInput7 $ playBingoStep 7 testBingoBoard1)

testWinningBoard = TestCase (assertEqual "winning board" (24, testBingoBoard3WinState) (play testInput [testBingoBoard1, testBingoBoard2, testBingoBoard3]))