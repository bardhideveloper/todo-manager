module Types
  ( Priority (..),
    Status (..),
    Task (..),
    TaskList,
  )
where

-- | Represents the priority level of a task.
data Priority = Low | Medium | High
  deriving (Show, Read, Eq, Ord)

-- | Represents whether a task is completed or still pending.
data Status = Pending | Completed
  deriving (Show, Read, Eq)

-- | A single task in the system.
data Task = Task
  { taskId :: Int,
    title :: String,
    description :: String,
    priority :: Priority,
    deadline :: Maybe String,
    status :: Status
  }
  deriving (Show, Read, Eq)

-- | Our task list as an immutable list of tasks.
type TaskList = [Task]