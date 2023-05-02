import * as common_strings from "./../strings";
import * as common_config_aws from "./aws";
import * as ADL from "@adl-lang/runtime/adl";

/* @generated from adl module common.config.sms */

export interface SmsConfig_Fake {
  kind: 'fake';
}
export interface SmsConfig_Twilio {
  kind: 'twilio';
  value: TwilioConfig;
}
export interface SmsConfig_Sns {
  kind: 'sns';
  value: AwsSnsConfig;
}

/**
 * Configuration for the sms provider used
 */
export type SmsConfig = SmsConfig_Fake | SmsConfig_Twilio | SmsConfig_Sns;

export interface SmsConfigOpts {
  /**
 * Fake sms, just prints outgoing message info to stdout
   */
fake: null;
  /**
 * Twilio implementation
   */
twilio: TwilioConfig;
  /**
 * AWS SNS  implementation
   */
sns: AwsSnsConfig;
}

export function makeSmsConfig<K extends keyof SmsConfigOpts>(kind: K, value: SmsConfigOpts[K]) { return {kind, value}; }

const SmsConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.sms","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"fake","default":{"kind":"nothing"},"name":"fake","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"twilio","default":{"kind":"nothing"},"name":"twilio","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.sms","name":"TwilioConfig"}},"parameters":[]}},{"annotations":[],"serializedName":"sns","default":{"kind":"nothing"},"name":"sns","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.sms","name":"AwsSnsConfig"}},"parameters":[]}}]}},"name":"SmsConfig","version":{"kind":"nothing"}}};

export const snSmsConfig: ADL.ScopedName = {moduleName:"common.config.sms", name:"SmsConfig"};

export function texprSmsConfig(): ADL.ATypeExpr<SmsConfig> {
  return {value:{typeRef:{kind:"reference",value:snSmsConfig},parameters:[]}};
}

/**
 * Configuration for twilio
 */
export interface TwilioConfig {
  accountSid: common_strings.StringNE;
  authToken: common_strings.StringNE;
}

export function makeTwilioConfig(
  input: {
    accountSid: common_strings.StringNE,
    authToken: common_strings.StringNE,
  }
): TwilioConfig {
  return {
    accountSid: input.accountSid,
    authToken: input.authToken,
  };
}

const TwilioConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.sms","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"accountSid","default":{"kind":"nothing"},"name":"accountSid","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.strings","name":"StringNE"}},"parameters":[]}},{"annotations":[],"serializedName":"authToken","default":{"kind":"nothing"},"name":"authToken","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.strings","name":"StringNE"}},"parameters":[]}}]}},"name":"TwilioConfig","version":{"kind":"nothing"}}};

export const snTwilioConfig: ADL.ScopedName = {moduleName:"common.config.sms", name:"TwilioConfig"};

export function texprTwilioConfig(): ADL.ATypeExpr<TwilioConfig> {
  return {value:{typeRef:{kind:"reference",value:snTwilioConfig},parameters:[]}};
}

/**
 * Configuration for AWS SMS implementation
 */
export interface AwsSnsConfig {
  credentials: common_config_aws.AwsCredentialsProvider;
  region: common_config_aws.AwsRegionProvider;
}

export function makeAwsSnsConfig(
  input: {
    credentials: common_config_aws.AwsCredentialsProvider,
    region: common_config_aws.AwsRegionProvider,
  }
): AwsSnsConfig {
  return {
    credentials: input.credentials,
    region: input.region,
  };
}

const AwsSnsConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.sms","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"credentials","default":{"kind":"nothing"},"name":"credentials","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.aws","name":"AwsCredentialsProvider"}},"parameters":[]}},{"annotations":[],"serializedName":"region","default":{"kind":"nothing"},"name":"region","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.aws","name":"AwsRegionProvider"}},"parameters":[]}}]}},"name":"AwsSnsConfig","version":{"kind":"nothing"}}};

export const snAwsSnsConfig: ADL.ScopedName = {moduleName:"common.config.sms", name:"AwsSnsConfig"};

export function texprAwsSnsConfig(): ADL.ATypeExpr<AwsSnsConfig> {
  return {value:{typeRef:{kind:"reference",value:snAwsSnsConfig},parameters:[]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "common.config.sms.SmsConfig" : SmsConfig_AST,
  "common.config.sms.TwilioConfig" : TwilioConfig_AST,
  "common.config.sms.AwsSnsConfig" : AwsSnsConfig_AST
};
