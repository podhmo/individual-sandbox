import xml.etree.ElementTree as ET
from xml.dom import minidom

d = {
    "HogeTest": [
        {
            "classname": "HogeTest",
            "name": "testCase01",
            "status": "success",
            "time": "0.001"
        },
        {
            "classname":
            "HogeTest",
            "name":
            "testCase02",
            "status":
            "failure",
            "type":
            "java.lang.AssertionError",
            "time":
            "0.001",
            "message":
            "test failed",
            "stack":
            """\
at org.junit.Assert.fail(Assert.java:88)
at jp.classmethod.testing.examples.core.runner.DynamicTestsRunnerExample$1.invokeTest(DynamicTestsRunnerExample.java:33)
at jp.classmethod.testing.core.runner.DynamicTestsRunner.invokeTest(DynamicTestsRunner.java:86)
at jp.classmethod.testing.core.runner.DynamicTestsRunner.run(DynamicTestsRunner.java:68)
at org.eclipse.jdt.internal.junit4.runner.JUnit4TestReference.run(JUnit4TestReference.java:50)
at org.eclipse.jdt.internal.junit.runner.TestExecution.run(TestExecution.java:38)
at org.eclipse.jdt.internal.junit.runner.RemoteTestRunner.runTests(RemoteTestRunner.java:467)
at org.eclipse.jdt.internal.junit.runner.RemoteTestRunner.runTests(RemoteTestRunner.java:683)
at org.eclipse.jdt.internal.junit.runner.RemoteTestRunner.run(RemoteTestRunner.java:390)
at org.eclipse.jdt.internal.junit.runner.RemoteTestRunner.main(RemoteTestRunner.java:197)
"""
        },
        {
            "classname":
            "HogeTest",
            "name":
            "testCase03",
            "status":
            "error",
            "type":
            "java.lang.NullPointerException",
            "time":
            "0.001",
            "message":
            "NPE",
            "stack":
            """\
at jp.classmethod.testing.examples.core.runner.DynamicTestsRunnerExample$1.invokeTest(DynamicTestsRunnerExample.java:36)
at jp.classmethod.testing.core.runner.DynamicTestsRunner.invokeTest(DynamicTestsRunner.java:86)
at jp.classmethod.testing.core.runner.DynamicTestsRunner.run(DynamicTestsRunner.java:68)
at org.eclipse.jdt.internal.junit4.runner.JUnit4TestReference.run(JUnit4TestReference.java:50)
at org.eclipse.jdt.internal.junit.runner.TestExecution.run(TestExecution.java:38)
at org.eclipse.jdt.internal.junit.runner.RemoteTestRunner.runTests(RemoteTestRunner.java:467)
at org.eclipse.jdt.internal.junit.runner.RemoteTestRunner.runTests(RemoteTestRunner.java:683)
at org.eclipse.jdt.internal.junit.runner.RemoteTestRunner.run(RemoteTestRunner.java:390)
at org.eclipse.jdt.internal.junit.runner.RemoteTestRunner.main(RemoteTestRunner.java:197)
"""
        },
        {
            "classname": "HogeTest",
            "name": "testCase03",
            "status": "skipped",
            "time": "0.000"
        },
    ]
}


def prettify(elem):
    """Return a pretty-printed XML string for the Element.
    """
    rough_string = ET.tostring(elem, 'utf-8')
    reparsed = minidom.parseString(rough_string)
    return reparsed.toprettyxml(indent="  ")


def dump(d):
    tree = ET.Element("testsuites")
    total = 0
    for sname, cases in d.items():
        suite = ET.SubElement(tree, "testsuite")
        stotal = 0
        suite.set("name", sname)
        for val in cases:
            case = ET.SubElement(suite, "testcase")
            case.set("name", val["name"])
            case.set("classname", val.get("classname") or val["name"])
            if "time" in val:
                case.set("time", str(val["time"]))

            status = val["status"]
            if status == "failure":
                failure = ET.SubElement(case, "failure")
                failure.set("type", val["type"])
                failure.set("message", val["message"])
                failure.text = val["stack"]
            elif status == "error":
                error = ET.SubElement(case, "error")
                error.set("type", val["type"])
                error.set("message", val["message"])
                error.text = val["stack"]
            elif status == "skipped":
                ET.SubElement(case, "skipped")
        stotal += len(cases)
        suite.set("tests", str(stotal))
        total += stotal
    tree.set("tests", str(total))
    return prettify(tree)


print(dump(d))
