import Data.Char
import Data.List

--TF(t) = Number of times term t appears in document
--IDF(t) = log2(Total number of documents / Number of documents with term t in it)
--TF-IDF = TF(t) * IDF(t)

--TF(t)
tf term doc = (numberOfTimesWithTermInDoc term doc) 
    where
        numberOfTimesWithTermInDoc term doc = fromIntegral $ length $ filter (\x -> x == term) doc

--IDF(t)
idf term docs = logBase 2 $ (numberOfDocs docs) / (numberOfDocsWithTerm term docs)
    where
        --Total number of documents
        numberOfDocs documents = fromIntegral $ length documents
        --Number of documents with term t in it
        numberOfDocsWithTerm term documents = sum $ map (\doc -> if isTermInDoc term doc then 1 else 0) (putDocToList documents)
        --putDocToList puts each document to a list; firstly upperToLower is applied
        putDocToList documents = map (\doc -> words $ upperToLower doc) documents
        --isTermInDoc checks whether a term is in a document
        isTermInDoc term doc = elem term $ filter (\x -> x == term) doc

--TF-IDF
tfidf term doc docs = (tf term doc) * (idf term docs)

--upperToLower converts uppercase letters to lowercase
upperToLower [] = []
upperToLower (x : xs) = toLower x : upperToLower xs

doc = words $ upperToLower "The quick brown fox jumps over the lazy dog"
docs = ["the quick brown fox jumps over the lazy dog", "the house is painted in white", "This project is falling behind schedule"]