package ${PackageName};

import android.app.Activity;

import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;

import androidx.test.espresso.action.ViewActions;
import androidx.test.espresso.matcher.ViewMatchers;
import androidx.test.ext.junit.rules.ActivityScenarioRule;
import androidx.test.ext.junit.runners.AndroidJUnit4;
import androidx.test.filters.LargeTest;

import static androidx.test.espresso.Espresso.onView;
import static androidx.test.espresso.action.ViewActions.click;
import static androidx.test.espresso.action.ViewActions.closeSoftKeyboard;
import static androidx.test.espresso.action.ViewActions.typeText;
import static androidx.test.espresso.assertion.ViewAssertions.matches;
import static androidx.test.espresso.matcher.ViewMatchers.withId;
import static androidx.test.espresso.matcher.ViewMatchers.withText;


@RunWith(AndroidJUnit4.class)
@LargeTest
public class SampleUiTest {

  @Rule public ActivityScenarioRule<MainActivity> activityScenarioRule
      = new ActivityScenarioRule<>(MainActivity.class);

  @Test
  public void changeText_sameActivity() {
    onView(withId(R.id.word)).check(matches(withText("你好哇")));
    onView(withId(R.id.change)).perform(click());
    onView(withId(R.id.word)).check(matches(withText("世界")));
  }
}
