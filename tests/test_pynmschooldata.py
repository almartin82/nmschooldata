"""
Tests for pynmschooldata Python wrapper.

Minimal smoke tests - the actual data logic is tested by R testthat.
These just verify the Python wrapper imports and exposes expected functions.
"""

import pytest


def test_import_package():
    """Package imports successfully."""
    import pynmschooldata
    assert pynmschooldata is not None


def test_has_fetch_enr():
    """fetch_enr function is available."""
    import pynmschooldata
    assert hasattr(pynmschooldata, 'fetch_enr')
    assert callable(pynmschooldata.fetch_enr)


def test_has_get_available_years():
    """get_available_years function is available."""
    import pynmschooldata
    assert hasattr(pynmschooldata, 'get_available_years')
    assert callable(pynmschooldata.get_available_years)


def test_has_version():
    """Package has a version string."""
    import pynmschooldata
    assert hasattr(pynmschooldata, '__version__')
    assert isinstance(pynmschooldata.__version__, str)
