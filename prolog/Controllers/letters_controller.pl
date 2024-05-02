start_letters(['<','<','<','A','A','A','A','A','A','A','A','A','A','A','A',
    'A','A','E','E','E','E','E','E','E','E','E','E','E','I','I','I','I','I',
    'I','I','I','I','I','O','O','O','O','O','O','O','O','O','O','S','S','S',
    'S','S','S','S','S','U','U','U','U','U','U','U','M','M','M','M','M','M',
    'R','R','R','R','R','R','T','T','T','T','T','D','D','D','D','D','L','L',
    'L','L','L','C','C','C','C','C','C','P','P','P','P','N','N','N','N','B',
    'B','B','F','F','G','G','H','H','V','V','J','J','Q','X','Z']).

letter_score('<', 0).
letter_score('A', 1).
letter_score('E', 1).
letter_score('I', 1).
letter_score('O', 1).
letter_score('S', 1).
letter_score('U', 1).
letter_score('M', 1).
letter_score('R', 1).
letter_score('T', 1).
letter_score('D', 2).
letter_score('L', 2).
letter_score('C', 2).
letter_score('P', 2).
letter_score('N', 3).
letter_score('B', 3).
letter_score('F', 4).
letter_score('G', 4).
letter_score('H', 4).
letter_score('V', 4).
letter_score('J', 5).
letter_score('Q', 6).
letter_score('X', 7).
letter_score('Z', 7).
letter_score(_, -1).