{-# LANGUAGE OverloadedStrings #-}

module LSP (diagnosticsLoop, initRepsonseFromRequest, definitionResponse) where

import           Language.Haskell.LSP.Types.Capabilities hiding(_experimental, _colorProvider, _workspace)
import           Language.Haskell.LSP.Types
import           Language.Haskell.LSP.Types as L

diagnosticsLoop :: [Uri] -> [PublishDiagnosticsNotification] -> ([PublishDiagnosticsNotification], [Uri])
diagnosticsLoop store diagnostics = do
    let diagWithError = filter diagnosticErrorExist diagnostics
    let diagWithErrorUri = map uriFromPublishDiagnosticsNotification diagWithError
    let cleanedDiagnostic = filter (\e -> elem (uriFromPublishDiagnosticsNotification e) store && notElem (uriFromPublishDiagnosticsNotification e) diagWithErrorUri) diagnostics
    let notCleanedYet = filter (\e -> notElem e (map uriFromPublishDiagnosticsNotification cleanedDiagnostic)) store
    let toSend = cleanedDiagnostic ++ diagWithError
    let newStore =notCleanedYet ++ diagWithErrorUri
    (toSend, newStore)

diagnosticErrorExist :: PublishDiagnosticsNotification -> Bool
diagnosticErrorExist = (\d -> case d of NotificationMessage _ _ params -> case params of PublishDiagnosticsParams _ diagnostics -> not (null diagnostics))

uriFromPublishDiagnosticsNotification :: PublishDiagnosticsNotification -> Uri
uriFromPublishDiagnosticsNotification notification = case notification of NotificationMessage _ _ param -> uriFromPublishDiagnosticsParams param

uriFromPublishDiagnosticsParams :: PublishDiagnosticsParams -> Uri
uriFromPublishDiagnosticsParams param = case param of PublishDiagnosticsParams uri _ -> uri

initRepsonseFromRequest :: InitializeRequest -> InitializeResponse
initRepsonseFromRequest request = case request of
  (RequestMessage _ origId _ _) ->
    ResponseMessage
      "2.0"
      (responseId origId)
      (Just $ InitializeResponseCapabilities serverCapabilities)
      Nothing


definitionResponse :: DefinitionRequest -> Maybe L.Location -> DefinitionResponse
definitionResponse request maybeLocation = case request of
  (RequestMessage _ origId _ _) ->
    ResponseMessage
      "2.0"
      (responseId origId)
      (Just $
        case maybeLocation of
          Nothing -> MultiLoc []
          Just loc -> SingleLoc loc)
      Nothing


-- No serverCapabilities at all for now
serverCapabilities =
  InitializeResponseCapabilitiesInner
    { _textDocumentSync                 = Nothing
    , _hoverProvider                    = Just False
    , _completionProvider               = Nothing
    , _signatureHelpProvider            = Nothing
    , _definitionProvider               = Just True
    , _typeDefinitionProvider           = Nothing
    , _implementationProvider           = Nothing
    , _referencesProvider               = Just False
    , _documentHighlightProvider        = Just False
    , _documentSymbolProvider           = Just False
    , _workspaceSymbolProvider          = Just False
    , _codeActionProvider               = Just False
    , _codeLensProvider                 = Nothing
    , _documentFormattingProvider       = Just False
    , _documentRangeFormattingProvider  = Just False
    , _documentOnTypeFormattingProvider = Nothing
    , _renameProvider                   = Just False
    , _documentLinkProvider             = Nothing
    , _colorProvider                    = Nothing
    , _foldingRangeProvider             = Nothing
    , _executeCommandProvider           = Nothing
    , _workspace                        = Nothing
    , _experimental                     = Nothing
    }
