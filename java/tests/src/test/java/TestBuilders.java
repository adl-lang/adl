import org.junit.Test;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;


import adl.test3.A;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;

public class TestBuilders {
  @Test
  public void testValidBuilderWorks(){
    new A.Builder()
      .setF_int((short)5)
      .setF_string("xx")
      .create();

    new A.Builder()
      .setF_int((short)5)
      .setF_string("xx")
      .setF_bool(true)
      .create();
  }

  @Test
  public void testInvalidBuilderThrows(){
    try {
      new A.Builder().create();
      throw new RuntimeException("BUG: shouldn't get here");
    } catch (IllegalStateException e) {
    }
  }
};
