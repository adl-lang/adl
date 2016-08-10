package org.adl.runtime;

import com.google.gson.JsonElement;

import java.io.IOException;

/**
 *  Interface for GSON based serialisation.
 */
public interface JsonBinding<T>
{
  Factory<T> factory();
  JsonElement toJson( T value );
  T fromJson( JsonElement json ); 
};

