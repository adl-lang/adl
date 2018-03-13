package org.adl.runtime;


/**
 * Helper functions for the implementation of Builders
 */
public class Builders {

  static public void checkFieldInitialized(String type, String field, Object value) {
    if (value == null) {
      throw new IllegalStateException("field " + field + " not initialized before calling " + type + ".Builder.create");
    }
  }

};
