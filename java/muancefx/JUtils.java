package muancefx;

import javafx.scene.control.TextField;

public class JUtils {
  /*
   * I could not find a Clojure syntax for this because when manipulating classes, Clojure always
   * ends up by calling Class/forName with initialize=true, which javafx does not like when the toolkit
   * is not initialized yet
   */
  public static boolean isAssignableFromTextField(String className) throws ClassNotFoundException {
    return TextField.class.isAssignableFrom(Class.forName(className, false, JUtils.class.getClassLoader()));
  }
}
