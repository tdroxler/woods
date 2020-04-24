{-# LANGUAGE OverloadedStrings #-}

module LSP (initRepsonseFromRequest, definitionResponse, referencesResponse) where

import           Language.Haskell.LSP.Types.Capabilities hiding(_experimental, _colorProvider, _workspace)
import           Language.Haskell.LSP.Types

initRepsonseFromRequest :: InitializeRequest -> InitializeResponse
initRepsonseFromRequest request = case request of
  (RequestMessage _ origId _ _) ->
    ResponseMessage
      "2.0"
      (responseId origId)
      (Just $ InitializeResponseCapabilities serverCapabilities)
      Nothing


definitionResponse :: DefinitionRequest -> [Location] -> DefinitionResponse
definitionResponse request locations = case request of
  (RequestMessage _ origId _ _) ->
    ResponseMessage
      "2.0"
      (responseId origId)
      (Just $
        case locations of
          (location : []) -> SingleLoc location
          _ -> MultiLoc locations)
      Nothing

referencesResponse :: ReferencesRequest -> [Location] -> ReferencesResponse
referencesResponse request locations = case request of
  (RequestMessage _ origId _ _) ->
    ResponseMessage
      "2.0"
      (responseId origId)
      (Just $ List locations)
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
    , _referencesProvider               = Just True
    , _documentHighlightProvider        = Just False
    , _documentSymbolProvider           = Just False
    , _workspaceSymbolProvider          = Just False
    , _codeActionProvider               = Nothing
    , _codeLensProvider                 = Nothing
    , _documentFormattingProvider       = Just False
    , _documentRangeFormattingProvider  = Just False
    , _documentOnTypeFormattingProvider = Nothing
    , _renameProvider                   = Nothing
    , _documentLinkProvider             = Nothing
    , _colorProvider                    = Nothing
    , _foldingRangeProvider             = Nothing
    , _executeCommandProvider           = Nothing
    , _workspace                        = Nothing
    , _experimental                     = Nothing
    }
