import org.junit.Test;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import adl.test2.Tree;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;

public class TestArrayRecursion {

  @Test
  public void testFactory(){
    Factory<Tree<String>> factory = Tree.factory(Factories.STRING);
  }

  @Test
  public void testJsonSerializer(){
    JsonBinding<Tree<String>> jbinding = Tree.jsonBinding(JsonBindings.STRING);
  }

};
