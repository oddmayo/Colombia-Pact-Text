library(readtext)

directorio <- 'C:\\Users\\ScmayorquinS\\OneDrive - Departamento Nacional de Planeacion\\DIDE\\2019\\Data Science Projects\\Colombia-Pact-Text'

#prueba <- readtext("C:\\Users\\ScmayorquinS\\Downloads\\05. PND CTI_13 enero 2019.docx")

texto_PND <- read_file("C:\\Users\\ScmayorquinS\\Desktop\\PND.txt")

# Separe cada texto después de un "*."
# EXCELENTE
texto_PND <- unlist(strsplit(texto_PND, "(?<=\\*.)\\s(?=[A-Z])", perl = T))
texto_PND <- texto_PND[2:6]

# Limpieza
