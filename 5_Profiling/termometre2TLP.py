import pandas as pd
import re as re


REDUCED = True

R_file = "./5_Profiling/ClassPanelGraph.R"
f1 = "./5_Profiling/Termometre_num_reduced.xlsx"
f2 = "./5_Profiling/Termometre_cat_reduced.xlsx"

#Abrir y guardar archivo
fr = open(R_file, "r")
lines = fr.read()
fr.close()

#Borrar archivo completo
fw = open(R_file, "w")
fw.close()


#Leer excels con datos para los termómetros numéricos y categóricos
#Guardar la información de los excels en un string (str_)
file_ = f1 if REDUCED else "./5_Profiling/Termometre_num.xlsx"
df = pd.read_excel(file_)

str_ = ""
CPG_vectors = ""

df = df.transpose()
df = df[df[0].notna()]

str_ += "numeric_cols <- list("
CPG_vectors = "CPG_variables <- list("
for col in df.columns:
    str_ += "list("
    cc = 0
    for el in df[col]:
        if cc==1 or cc==4:
            pass
        elif re.match(r'\d|-\d', str(el)):
            str_ += f'{el},'
        else:
            str_ += f'"{el}",'
            CPG_vectors += f'"{el}",'
        cc+=1
    str_ = str_[0:len(str_)-1]
    str_ += "),\n   "
str_ = str_[0:len(str_)-5]
str_ += ")\n\n"



file_cat = f2 if REDUCED else "./5_Profiling/Termometre_cat.xlsx"
df = pd.read_excel(file_cat)

df = df.transpose()
df = df[df[2].notna()]


str_ += "categoric_cols <- list("
for col in df.columns:
    str_ += "list("
    cc = 0
    for el in df[col]:
        el_str = str(el)
        if cc == 0:
            str_ += f'"{el}",'
            CPG_vectors += f'"{el}",'
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
    str_ += "),\n   "
str_ = str_[0:len(str_)-5]
str_ += ")\n\n"
CPG_vectors = CPG_vectors[0:len(CPG_vectors)-1]
CPG_vectors += ")"

#Escribir en el archivo los vectores verde y rojos
fw = open(R_file, "w")
lines = re.sub(r'VECTORES NUMERICOS Y CATEGORICOS(\n(.)*)*FIN VECTORES', f'VECTORES NUMERICOS Y CATEGORICOS\n {str_} \n # FIN VECTORES', lines) 
lines = re.sub(r'VECTORES CLASS PANEL GRAPH(\n(.)*)* FIN CPGVECTORES', f'VECTORES CLASS PANEL GRAPH\n {CPG_vectors} \n# FIN CPGVECTORES', lines)
if REDUCED:
    lines = re.sub(r'VAR REDUCED(\n(.)*)*FIN REDUCED', f'VAR REDUCED\nREDUCED = TRUE \n# FIN REDUCED', lines)
else:
    lines = re.sub(r'VAR REDUCED(\n(.)*)*FIN REDUCED', f'VAR REDUCED\nREDUCED = FALSE \n# FIN REDUCED', lines)


fw.write(lines)
fw.close()
