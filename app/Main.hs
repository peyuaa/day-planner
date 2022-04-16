{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as Text
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

data Status = NotCompleted | Completed deriving (Show, Eq)

type Task =  Text
data TaskEntity = TaskEntity Task Status deriving (Show, Eq)

data Model = Model
  { todoLists   :: HashMap Text [TaskEntity]
  , currentList :: Text
  }

defaultListName :: Text
defaultListName = "Day Plans"

initialModel :: Model
initialModel = Model
  { todoLists = HashMap.fromList [(defaultListName, [])]
  , currentList = defaultListName
  }

data Action
  =  Start
  | AddTask Task
  | RemoveTask Task
  | SwitchToList Text
  | ShowAll
  | Show Text
  | MarkCompleted Text
  deriving (Show, Read)

todoBot3 :: BotApp Model Action
todoBot3 = BotApp
  { botInitialModel = initialModel
  , botAction = flip updateToAction
  , botHandler = handleAction
  , botJobs = []
  }
  where
    updateToAction :: Model -> Update -> Maybe Action
    updateToAction _ = parseUpdate $
          AddTask      <$> plainText
      <|> Start        <$  command "start"
      <|> AddTask      <$> command "add"
      <|> RemoveTask   <$> command "remove"
      <|> SwitchToList <$> command "switch_to_list"
      <|> Show         <$> command "show"
      <|> ShowAll      <$  command "show_all"
      <|> MarkCompleted <$> command "mark_completed"
      <|> callbackQueryDataRead

    handleAction :: Action -> Model -> Eff Action Model
    handleAction action model = case action of
      Start -> model <# do
        reply (toReplyMessage startMessage)
          { replyMessageReplyMarkup = Just (SomeReplyKeyboardMarkup startKeyboard) }
      AddTask task -> addTask task model NotCompleted <# do
        replyText "Task added!"
      RemoveTask task -> removeTask task model <# do
        replyText "Task removed!"  
      MarkCompleted task -> markCompleted task model <# do
        replyText "Task completed!"
      SwitchToList name -> model { currentList = name } <# do
        replyText ("Switched to list «" <> name <> "»!") 
      ShowAll -> model <# do
        reply (toReplyMessage "Available todo lists")
          { replyMessageReplyMarkup = Just (SomeInlineKeyboardMarkup listsKeyboard) }
      Show "" -> model <# do
        return (Show (currentList model))
      Show name -> model <# do
        let tasks = concat (HashMap.lookup name (todoLists model))
        if null tasks
          then reply (toReplyMessage ("The list «" <> name <> "» is empty. Maybe try these starter options?"))
                 { replyMessageReplyMarkup = Just (SomeReplyKeyboardMarkup startKeyboard) }
          else replyText (Text.replace "Completed" "✅" (Text.replace "NotCompleted" "❌" (Text.unlines ((map ((Text.drop 11) . Text.pack . show) tasks)))))

      where
        listsKeyboard = InlineKeyboardMarkup
          (map (\name -> [actionButton name (Show name)]) (HashMap.keys (todoLists model)))

    startMessage = Text.unlines
      [ "Hello! I'm your personal planner bot!"
      , ""
      , "You can add new tasks to your to-do list just by typing it!"
      , "You can also use /add <t command to do that explicitely."
      , "To remove an task use /remove <task> command."
      , "To mark a task as completed, use /mark_completed <task>."
      , ""
      , "You can manage multiple to-do lists:"
      , "Switch to a new named list with /switch_to_list <list>."
      , "Show all available lists with /show_all."
      , "Show tasks for a specific list with /show <list>."
      , ""
      , "Here are some starter options, try adding something to the list."
      ]

    startKeyboard :: ReplyKeyboardMarkup
    startKeyboard = ReplyKeyboardMarkup
      { replyKeyboardMarkupKeyboard =
          [ [ "Plant a dog", "Build a tree" ]
          , [ "Burn the roof of the house", "Emigrate to Georgia" ]
          ]
      , replyKeyboardMarkupResizeKeyboard = Just True
      , replyKeyboardMarkupOneTimeKeyboard = Just True
      , replyKeyboardMarkupSelective = Nothing
      , replyKeyboardMarkupInputFieldSelector = Nothing
      }

addTask :: Task -> Model -> Status -> Model
addTask task model status = model
  { todoLists = HashMap.insertWith (++) (currentList model) ([TaskEntity task status]) (todoLists model) }

removeTask :: Task -> Model -> Model
removeTask task model = model
  { todoLists = HashMap.adjust (filter (/= (TaskEntity task NotCompleted))) (currentList model) (todoLists model)}

markCompleted :: Task -> Model -> Model
markCompleted task model = (addTask task (removeTask task model) Completed)

run :: Token -> IO ()
run token = do
  env <- defaultTelegramClientEnv token
  startBot_ (conversationBot updateChatId todoBot3) env

main :: IO ()
main = do
--   putStrLn "Enter Telegram bot's API token:"
--   token <- Token . Text.pack <$> getLine
  run "5164613829:AAFUFcFj0ZzWtUhQWCijPIIx6e5nhky2oyw"