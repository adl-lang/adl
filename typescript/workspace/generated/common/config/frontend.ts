import * as ADL from "@adl-lang/runtime/adl";

/* @generated from adl module common.config.frontend */

/**
 * Configuration for a consumer which processes work from a queue with a bounded thread pool.
 *
 * e.g. Useful for configuring jetty's request thread pool.
 */
export interface ThreadPoolConfig {
/**
 * Maximum number of threads that can be spawned to handle requests
 */
  maxThreads: number;
/**
 * Minimum number of threads ready for jetty to handle requests
 */
  minThreads: number;
/**
 * How long a thread can be idle before it is candidate to be stopped
 */
  idleTimeoutMillis: number;
/**
 * Maximum number of pending requests before new ones are dropped
 */
  queueSize: number;
}

export function makeThreadPoolConfig(
  input: {
    maxThreads: number,
    minThreads: number,
    idleTimeoutMillis: number,
    queueSize: number,
  }
): ThreadPoolConfig {
  return {
    maxThreads: input.maxThreads,
    minThreads: input.minThreads,
    idleTimeoutMillis: input.idleTimeoutMillis,
    queueSize: input.queueSize,
  };
}

const ThreadPoolConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.frontend","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"maxThreads","default":{"kind":"nothing"},"name":"maxThreads","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}},{"annotations":[],"serializedName":"minThreads","default":{"kind":"nothing"},"name":"minThreads","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}},{"annotations":[],"serializedName":"idleTimeoutMillis","default":{"kind":"nothing"},"name":"idleTimeoutMillis","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}},{"annotations":[],"serializedName":"queueSize","default":{"kind":"nothing"},"name":"queueSize","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}}]}},"name":"ThreadPoolConfig","version":{"kind":"nothing"}}};

export const snThreadPoolConfig: ADL.ScopedName = {moduleName:"common.config.frontend", name:"ThreadPoolConfig"};

export function texprThreadPoolConfig(): ADL.ATypeExpr<ThreadPoolConfig> {
  return {value:{typeRef:{kind:"reference",value:snThreadPoolConfig},parameters:[]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "common.config.frontend.ThreadPoolConfig" : ThreadPoolConfig_AST
};
