library(TKCat)
library(Matrix)

sm <- Matrix(rnorm(10000000), nrow=100000, ncol=10, sparse=TRUE)
sm <- summary(sm) %>%
   as_tibble() %>% 
   slice(sample.int(10^6, 10^4))
sm <- sparseMatrix(i=sm$i, j=sm$j, x=sm$x)
rownames(sm) <- paste0("r", 1:nrow(sm))
colnames(sm) <- paste0("c", 1:ncol(sm))

mc <- tibble(cn=colnames(sm), info=sample(letters, ncol(sm), replace=TRUE))
mr <- tibble(rn=rownames(sm), info=sample(LETTERS, nrow(sm), replace=TRUE))

fm <- matrix(sample(c(TRUE, FALSE), 54, replace=TRUE), nrow=9, ncol=6)
rownames(fm) <- paste0("r", 1:nrow(fm))
colnames(fm) <- paste0("c", 1:ncol(fm))

m <- df_to_model(sm, mc, mr, fm) %>% auto_layout()
m <- ReDaMoR::add_foreign_key(m, fromTable="sm", fromFields="row", toTable="mr", toFields="rn")
m <- ReDaMoR::add_foreign_key(m, fromTable="sm", fromFields="column", toTable="mc", toFields="cn")
m <- ReDaMoR::add_foreign_key(m, fromTable="fm", fromFields="row", toTable="mr", toFields="rn")
m <- ReDaMoR::add_foreign_key(m, fromTable="fm", fromFields="column", toTable="mc", toFields="cn")


mdb <- memoMDB(
   dataTables=list(sm=sm, mc=mc, mr=mr, fm=fm),
   dataModel=m,
   dbInfo=list(name="stest")
)
count_records(mdb)
dims(mdb)
fmdb <- as_fileMDB(mdb, path="~/Tmp/")
