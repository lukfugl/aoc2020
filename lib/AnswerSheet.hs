module AnswerSheet (AnswerSheet, GroupAnswers) where

import Text.ParserCombinators.ReadP
import Text.Read.Lex

type AnswerSheet = String
type GroupAnswers = [AnswerSheet]
