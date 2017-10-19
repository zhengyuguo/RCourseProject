source('extremecosine.R')
disease=data.frame(rnorm(10,0,3))
rownames(disease)=LETTERS[4:13]
drug=data.frame(rnorm(10,0,3))
rownames(drug)=LETTERS[4:13]

extremecosine(disease,drug)
