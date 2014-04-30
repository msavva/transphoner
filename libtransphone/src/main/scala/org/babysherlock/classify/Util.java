package org.babysherlock.classify;

import edu.stanford.nlp.util.StringUtils;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Utility functions
 *
 * @author Angel Chang
 */
public class Util {
  public static String getProperty(Properties props, String prefix, String key)
  {
    return props.getProperty((prefix != null)? prefix + "." + key:key);
  }

  public static String getProperty(Properties props, String prefix, String key, String defaultVal)
  {
    return props.getProperty((prefix != null)? prefix + "." + key:key, defaultVal);
  }

  private static final Pattern varPattern = Pattern.compile("\\$\\{(.*?)\\}");
  public static Properties expandPropertyVars(Properties props)
  {
    // Create a new Properties with variables expanded
    Properties expanded = new Properties();
    for (String str:props.stringPropertyNames()) {
      String value = props.getProperty(str);
      Set<String> vars = new HashSet<String>();
      Matcher m = varPattern.matcher(value);
      while (m.find()) {
        vars.add(m.group(1));
      }
      String expandedValue = value;
      if (vars.size() > 0) {
        String[] expandedValues = new String[] { value };
        for (String var:vars) {
          String varValue = props.getProperty(var);
          String[] varValueExpanded = varValue.split("[,\\s]+");
          String[] unexpandedValues = expandedValues;
          expandedValues = new String[unexpandedValues.length*varValueExpanded.length];
          int i = 0;
          String replaceRegex = Pattern.quote("${" + var +"}");
          for (String v:varValueExpanded) {
            for (String ex:unexpandedValues) {
              expandedValues[i] = ex.replaceAll(replaceRegex, v);
              i++;
            }
          }
        }
        expandedValue = StringUtils.join(expandedValues, ",");
        System.err.println("Expanded property " + str + ": " + value + " to " + expandedValue);
      }
      expanded.put(str, expandedValue);
    }
    return expanded;
  }

}
