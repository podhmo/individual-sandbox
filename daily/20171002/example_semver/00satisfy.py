from semver import max_satisfying


def satisfy(versions, range_, result):
    sat = max_satisfying(versions, range_, loose=True)
    assert result == sat, "%s != %s" % (result, sat)

# If only pre-releases are available, then they should be returned
satisfy(['1.2.3-alpha.1'], '~1.2.3', '1.2.3-alpha.1')
satisfy(['1.2.3-alpha.1'], '~1.2.3-', '1.2.3-alpha.1')
satisfy(['1.2.3-pre.1', '1.2.3'], '~1.2.3', '1.2.3')
