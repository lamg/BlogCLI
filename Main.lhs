> {-# LANGUAGE UnicodeSyntax #-}

This module is the user's (command line) interface to
the Blog program. There must be a file named Blog.state
in the same directory this program is executed.

> module Main where
> import SimpleBlog.Blog (Blog, Comment, Post, visit, read, comment, post)
> import Prelude hiding (read)
> import SimpleBlog.Persistence (store, restore)
> import System.Environment (getArgs)
> import Data.Maybe (listToMaybe)
 
> main ∷ IO ()
> main = do c ← getCmd
>           b ← restore
>           exec c $! b

The type of exec and of getCmd must be the simplest
interface between both.

> -- Interfacing with the world has introduced some
> -- unpleasant complexity

> getCmd ∷ IO Cmd
> getCmd = getArgs >>= return . lookCmd commands . listToMaybe

> lookCmd ∷ [(String, Cmd)] → Maybe String → Cmd
> lookCmd cs x = maybe helpCmd id $ x >>= flip lookup cs

> commands ∷ [(String, Cmd)]
> commands = [("help", helpCmd), ("visit", visitCmd),
>             ("read", readCmd), ("comment", commentCmd),
>             ("post", postCmd)]

> type Cmd = Args → Blog → Either String Blog
> type Args = [String]
>
> helpCmd ∷ Cmd
> helpCmd _ _ = helpMsg
>
> visitCmd ∷ Cmd
> visitCmd [] = Left . show . visit
> visitCmd _ = (\_ → wrongArg)
> 
> readCmd ∷ Cmd
> readCmd [a] = Left . show . flip read a
> readCmd _ = (\_ → wrongArg)
> 
> commentCmd ∷ Cmd
> commentCmd [t, c] b = maybe errorMsg Right $
>                       (\pc → comment pc t b) . fst
>                       =<< listToMaybe (reads c ∷ [(Comment, String)])
> commentCmd _ _ = wrongArg
> 
> postCmd ∷ Cmd
> postCmd [a] b = maybe errorMsg Right $ (\pp → Just $ post b pp). fst =<< listToMaybe (reads a ∷ [(Post, String)])
> postCmd _ _ = wrongArg

> helpMsg ∷ Either String Blog
> helpMsg = Left "The following commands are\n\
> \defined:\n\
> \help         Prints this message.\n\
> \visit        Prints a summary of posts in this blog.\n\
> \read         Receives the title of a post and shows it.\n\
> \comment      Receives the title of a post and adds a comment to it.\n\
> \post         Receives a post and adds it to the blog."

> wrongArg ∷ Either String Blog
> wrongArg = Left "Wrong number of arguments."
> 
> errorMsg ∷ Either String Blog
> errorMsg = Left "Parsing error or operation error."

> exec ∷ (Cmd) → Blog → IO ()
> exec c b = either putStrLn store . flip c b .
>            (\l → if null l then l else tail l) =<< getArgs
