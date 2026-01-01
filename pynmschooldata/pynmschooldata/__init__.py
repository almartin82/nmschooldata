"""
pynmschooldata - Python wrapper for New Mexico school enrollment data.

Thin rpy2 wrapper around the nmschooldata R package.
Returns pandas DataFrames.
"""

from .core import (
    fetch_enr,
    fetch_enr_multi,
    tidy_enr,
    get_available_years,
)

__version__ = "0.1.0"
__all__ = ["fetch_enr", "fetch_enr_multi", "tidy_enr", "get_available_years"]
