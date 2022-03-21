{-# LANGUAGE OverloadedStrings #-}

module LSP (initRepsonseFromRequest) where --, definitionResponse, referencesResponse, renameResponse) where

import           Language.LSP.Types.Capabilities hiding(_experimental, _colorProvider, _workspace)
import           Language.LSP.Types



initRepsonseFromRequest :: RequestMessage 'Initialize -> Int
initRepsonseFromRequest request = case request of
  (RequestMessage _ origId _ _) ->
    ResponseMessage
      "2.0"
      (responseId origId)
      (Right $ InitializeResult serverCapabilities)


--definitionResponse request locations = case request of
--  (RequestMessage _ origId _ _) ->
--    ResponseMessage
--      "2.0"
--      (responseId origId)
--      (Right $
--        case locations of
--          (location : []) -> SingleLoc location
--          _ -> MultiLoc locations)

--referencesResponse request locations = case request of
--  (RequestMessage _ origId _ _) ->
--    ResponseMessage
--      "2.0"
--      (responseId origId)
--      (Right $ List locations)

--renameResponse request = case request of
--  (RequestMessage _ origId _ _) ->
--    ResponseMessage
--      "2.0"
--      (responseId origId)
--      Right Empty


---- No serverCapabilities at all for now
--serverCapabilities =
--  ServerCapabilities
--  { -- | Defines how text documents are synced. Is either a detailed structure
--      -- defining each notification or for backwards compatibility the
--      -- 'TextDocumentSyncKind' number.
--      -- If omitted it defaults to 'TdSyncNone'.
--      _textDocumentSync                 = Nothing
--      -- | The server provides hover support.
--  , _hoverProvider                    = Nothing
--      -- | The server provides completion support.
--  , _completionProvider               = Nothing
--      -- | The server provides signature help support.
--  , _signatureHelpProvider            = Nothing
--      -- | The server provides go to declaration support.
--      --
--      -- Since LSP 3.14.0
--  , _declarationProvider              = Nothing
--      -- | The server provides goto definition support.
--  , _definitionProvider               = Nothing
--      -- | The server provides Goto Type Definition support. Since LSP 3.6
--      --
--      -- @since 0.7.0.0
--  , _typeDefinitionProvider           = Nothing
--      -- | The server provides Goto Implementation support. Since LSP 3.6
--      --
--      -- @since 0.7.0.0
--  , _implementationProvider           = Nothing
--      -- | The server provides find references support.
--  , _referencesProvider               = Nothing
--      -- | The server provides document highlight support.
--  , _documentHighlightProvider        = Nothing
--      -- | The server provides document symbol support.
--  , _documentSymbolProvider           = Nothing
--      -- | The server provides code actions.
--  , _codeActionProvider               = Nothing
--      -- | The server provides code lens.
--  , _codeLensProvider                 = Nothing
--      -- | The server provides document link support.
--  , _documentLinkProvider             = Nothing
--      -- | The server provides color provider support. Since LSP 3.6
--      --
--      -- @since 0.7.0.0
--  , _colorProvider                    = Nothing
--      -- | The server provides document formatting.
--  , _documentFormattingProvider       = Nothing
--      -- | The server provides document range formatting.
--  , _documentRangeFormattingProvider  = Nothing
--      -- | The server provides document formatting on typing.
--  , _documentOnTypeFormattingProvider = Nothing
--      -- | The server provides rename support.
--  , _renameProvider                   = Nothing
--      -- | The server provides folding provider support. Since LSP 3.10
--      --
--      -- @since 0.7.0.0
--  , _foldingRangeProvider             = Nothing
--      -- | The server provides execute command support.
--  , _executeCommandProvider           = Nothing
--      -- | The server provides selection range support. Since LSP 3.15
--  , _selectionRangeProvider           = Nothing
--      -- | The server provides call hierarchy support.
--  , _callHierarchyProvider            = Nothing
--      -- | The server provides semantic tokens support.
--      --
--      -- @since 3.16.0
--  , _semanticTokensProvider           = Nothing
--      -- | The server provides workspace symbol support.
--  , _workspaceSymbolProvider          = Nothing
--      -- | Workspace specific server capabilities
--  , _workspace                        = Nothing
--      -- | Experimental server capabilities.
--  , _experimental                     = Nothing
--    }
