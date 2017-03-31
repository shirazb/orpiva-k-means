# orpiva-k-means
Implementation, in Haskell, of the k-means clustering challenge of Orpiva internship challenge week 2017.

'stack build' will build the project.
'stack ghci' to open a repl, or use ghci on a specific file.
'stack exec orpiva-k-means-exe' to execute the project.

# IMPORTANT
This is very incomplete. Parser performs almost no error reporting. The parser is hardly tested. The data is not extracted from the AST and fed into the K-means clustering algorithm. The graph plotting is not implemented at all.
