module Wheb.Plugins.Debug.MemoryBackend.Auth where

data UserData = UserData
  { userStorage :: TVar (M.Map UserKey (Maybe PwHash)) }

instance AuthTypes UserData
-- | In memory auth backend. User values
-- will not persist after server restart.
instance AuthBackend UserData where
  backendGetUser ui@(UserId key _type) (UserData tv) = do
        possUser <- liftM (M.lookup key) $ liftIO $ readTVarIO tv
        case possUser of
          Nothing -> return Nothing
          Just _ -> return $ Just (AuthUser ui ())
  backendLogin ui@(UserId key _type) claim (AuthCredentials (Just pw) _ _ _ _) (UserData tv) = do
        users <- liftIO $ readTVarIO tv
        runExceptT $ do
          maybePw <- maybe (throwE UserDoesNotExist) return $ M.lookup key users
          if maybe True (verifyPw pw) maybePw
             then return $ AuthUser ui claim
             else throwE InvalidPassword
  backendLogin _ _ _ _ = return $ Left AuthInternalError
  backendLogout _ =  getUserSessionKey >>= deleteSessionValue

instance AccountBackend UserData where
  backendRegister ui@(UserId name _) () (AuthCredentials pw _host _totp _dev _tok) (UserData tv) = do
    users <- liftIO $ readTVarIO tv
    if M.member name users
      then return $ Left DuplicateUsername
      else do
      maybePwHash <- maybe (return Nothing) (makePwHash >=> return . Just) pw
      liftIO $ atomically $ writeTVar tv (M.insert name maybePwHash users)
      return $ Right ui
  backendRegisterByUsernamePassword un pw = backendRegister (UserId un $ Just UserName) () (credsByPw pw)
  backendRegisterByEmailPassword em pw = backendRegister (UserId em $ Just EmailKey) () (credsByPw pw)
  backendRegisterByEmail em               = backendRegister (UserId em $ Just EmailKey) () emptyCreds
  backendUnregister (UserId name _) (UserData tv) = do
    users <- liftIO $ readTVarIO tv
    if not $ M.member name users
       then return $ Left AuthRegUserDoesNotExist
       else do
        liftIO $ atomically $ writeTVar tv (M.delete name users)
        return $ Right ()


initAuthMemory :: InitM g s m AuthContainer
initAuthMemory = do
  tv <- liftIO $ newTVarIO M.empty
  return $! AuthContainer $ UserData tv
