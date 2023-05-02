import {_AST_MAP as common} from "./";
import {_AST_MAP as common_adminui_api} from "./adminui/api";
import {_AST_MAP as common_adminui_config} from "./adminui/config";
import {_AST_MAP as common_adminui_db} from "./adminui/db";
import {_AST_MAP as common_config_aws} from "./config/aws";
import {_AST_MAP as common_config_azure} from "./config/azure";
import {_AST_MAP as common_config_db} from "./config/db";
import {_AST_MAP as common_config_emailer} from "./config/emailer";
import {_AST_MAP as common_config_frontend} from "./config/frontend";
import {_AST_MAP as common_config_google} from "./config/google";
import {_AST_MAP as common_config_jwt} from "./config/jwt";
import {_AST_MAP as common_config_log} from "./config/log";
import {_AST_MAP as common_config_okta} from "./config/okta";
import {_AST_MAP as common_config_sms} from "./config/sms";
import {_AST_MAP as common_config_storage} from "./config/storage";
import {_AST_MAP as common_db} from "./db";
import {_AST_MAP as common_flyway_api} from "./flyway/api";
import {_AST_MAP as common_flyway_example_patterns} from "./flyway/example_patterns";
import {_AST_MAP as common_flyway_internals} from "./flyway/internals";
import {_AST_MAP as common_http} from "./http";
import {_AST_MAP as common_strings} from "./strings";
import {_AST_MAP as common_tabular} from "./tabular";
import {_AST_MAP as common_ui} from "./ui";
import {ScopedDecl, declResolver} from "@adl-lang/runtime/adl";
import {ADL_local as adl_lang_sys} from "@adl-lang/sys/resolver";

/* @generated from adl */

export const ADL_local: { [key: string]: ScopedDecl } = {
  ...common,
  ...common_adminui_api,
  ...common_adminui_config,
  ...common_adminui_db,
  ...common_config_aws,
  ...common_config_azure,
  ...common_config_db,
  ...common_config_emailer,
  ...common_config_frontend,
  ...common_config_google,
  ...common_config_jwt,
  ...common_config_log,
  ...common_config_okta,
  ...common_config_sms,
  ...common_config_storage,
  ...common_db,
  ...common_flyway_api,
  ...common_flyway_example_patterns,
  ...common_flyway_internals,
  ...common_http,
  ...common_strings,
  ...common_tabular,
  ...common_ui,
};

export const ADL: { [key: string]: ScopedDecl } = {
  ...ADL_local,
  ...adl_lang_sys,
};

export const RESOLVER = declResolver(ADL);
