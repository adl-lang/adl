package org.adl.runtime;

import java.util.Arrays;
import java.util.Objects;

/**
 * Wrapper class for byte[].
 */
public class ByteArray {
  private byte[] value;
  
  public ByteArray() {
    value = new byte[0];
  }

  public ByteArray(byte value[]) {
    this.value = Objects.requireNonNull(value);
  }

  public ByteArray(ByteArray other) {
    this.value = Arrays.copyOf(other.value, other.value.length);
  }

  public byte [] getValue() {
    return this.value;
  }

  public void setValue(byte[] value) {
    Objects.requireNonNull(value);
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ByteArray)) {
      return false;
    }
    return Arrays.equals(value, ((ByteArray) other).value);
  }

  @Override
  public int hashCode() {
    return Arrays.hashCode(value);
  }
};
