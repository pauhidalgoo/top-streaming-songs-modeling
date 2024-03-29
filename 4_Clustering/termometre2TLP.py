import pandas as pd
import re as re

file_ = "./Termometro.xlsx"
df = pd.read_excel(file_)


str_ = ""
df = df.transpose()

df = df[df[0].notna()]


str_ += "list("
for col in df.columns:
    if df[col][0] == "FINAL_LINEA":
        break
    str_ += "   list("
    
    cc = 0
    for el in df[col]:
        if cc==1 or cc==4:
            pass
        elif re.match(str(el), r'/d'):
            str_ += f'{el},'
        else:
            str_ += f'"{el}",'
        cc+=1
    str_ = str_[0:len(str_)-1]
    str_ += "),\n"
str_ = str_[0:len(str_)-2]
str_ += ")"
print(str_)
