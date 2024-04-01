import pandas as pd
import re as re
'''
file_ = "./Termometre_num.xlsx"
df = pd.read_excel(file_)


str_ = ""
df = df.transpose()

df = df[df[0].notna()]

str_ += "numeric_cols <- "
str_ += "list("
for col in df.columns:
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
str_ += ")\n\n"
'''
str_ = ""
file_cat = "./Termometre_cat.xlsx"
df = pd.read_excel(file_cat)

print(df)

df = df.transpose()
df = df[df[2].notna()]

print(df)

str_ += "categoric_cols <- "
str_ += "list("
for col in df.columns:
    str_ += "   list("

    cc = 0
    for el in df[col]:
        el_str = str(el)

        if cc == 0:
            str_ += f'"{el}",'
        else:
            el_ll = el_str.split()

            str_ += "c("
            for mod in el_ll:
                if mod == "TRUE_val":
                    str_+=f'"TRUE",'
                elif mod == "FALSE_val":
                    str_+=f'"FALSE",'
                else:
                    str_ += f'"{mod}",'
            
            str_ = str_[0:len(str_)-1]
            str_ += "),"
        

        cc += 1

    str_ = str_[0:len(str_)-1]
    str_ += "),\n"
str_ = str_[0:len(str_)-2]
str_ += ")\n\n"

print(str_)




print(str_)
