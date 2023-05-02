import {_AST_MAP as protoclient_protoapp_api} from "./protoapp/api";
import {_AST_MAP as protoclient_protoapp_config} from "./protoapp/config";
import {_AST_MAP as protoclient_protoapp_db} from "./protoapp/db";
import {_AST_MAP as protoclient_protoapp_uiconfig} from "./protoapp/uiconfig";
import {ADL_local as adl_lang_common} from "@adl-lang/common/resolver";
import {ScopedDecl, declResolver} from "@adl-lang/runtime/adl";
import {ADL_local as adl_lang_sys} from "@adl-lang/sys/resolver";

/* @generated from adl */

export const ADL_local: { [key: string]: ScopedDecl } = {
  ...protoclient_protoapp_api,
  ...protoclient_protoapp_config,
  ...protoclient_protoapp_db,
  ...protoclient_protoapp_uiconfig,
};

export const ADL: { [key: string]: ScopedDecl } = {
  ...ADL_local,
  ...adl_lang_common,
  ...adl_lang_sys,
};

export const RESOLVER = declResolver(ADL);
