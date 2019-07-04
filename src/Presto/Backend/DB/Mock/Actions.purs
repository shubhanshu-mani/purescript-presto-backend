module Presto.Backend.DB.Mock.Actions where

import Presto.Backend.Language.Types.DB ()
import Presto.Backend.DB.Mock.Types ()

data FindOne         = FindOne
data FindAll         = FindAll
data Update          = Update
data Delete          = Delete
data CreateWithOpts  = CreateWithOpts
data Create          = Create
data Query           = Query


mkFindOne        dbName = FindOne
mkFindAll        dbName = FindAll
mkUpdate         dbName = Update
mkDelete         dbName = Delete
mkCreateWithOpts dbName = CreateWithOpts
mkCreate         dbName = Create
mkQuery          dbName = Query
