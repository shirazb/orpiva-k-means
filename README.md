# orpiva-k-means
Implementation, in Haskell, of the k-means clustering challenge of Orpiva internship challenge week 2017.

'stack build' will build the project.
'stack ghci' to open a repl, or use ghci on a specific file.
'stack exec orpiva-k-means-exe <path-to-arff-file>' to execute the project.

# IMPORTANT
This is very incomplete. Currently, executing the program will just dump the AST
of the given ARFF file.

Parser performs almost no error reporting. The parser is hardly tested.
The data is not extracted from the AST and fed into the K-means clustering
algorithm. The graph plotting is not implemented at all.
