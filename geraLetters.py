letters = [
    ["<"*3],
    ["A"*14,"E"*11,"I"*10,"O"*10,"S"*8,"U"*7,"M"*6,"R"*6,"T"*5],
    ["D"*5,"L"*5,"C"*4,"P"*4],
    ["N"*4, "B"*3, "Ç"*2],
    ["F"*2, "G"*2, "H"*2, "V"*2],
    ["J"*2],
    ["Q"*1],
    ["X"*1, "Z"*1]]
res = "["
for i in range(len(letters)):
    for c in letters[i]:
        for v in c:
            template = "Letter {letter='" + v + "', letterScore=" + str(i) +"},\n"
            res += template
res += "]"

print(res)