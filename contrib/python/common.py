#!/usr/bin/env python3
""" coding: utf-8
"""

from typing import Iterator, List, NamedTuple, Text, Tuple, Union

import os

from functools import partial

import h5py

from numpy import ndarray

# H5Path data constructors
H5PathContains = NamedTuple("H5PathContains", [("path", Text)])

H5PathOptionalItemValue = NamedTuple('H5OptionalItemValue', [('path', Text),
                                                             ('default', float)])  # noqa
H5PathWithAttribute = NamedTuple("H5PathWithAttribute", [('attribute', Text),
                                                         ('value', bytes)])

H5Path = Union[H5PathContains, H5PathOptionalItemValue, H5PathWithAttribute]

Parameter = NamedTuple("Parameter", [("name", Text),
                                     ("value", float),
                                     ("bounds", Tuple[float, float])])


def _v_attrs(attribute: Text, value: Text, _name: Text, obj) -> h5py.Dataset:
    """extract all the images and accumulate them in the acc variable"""
    if isinstance(obj, h5py.Dataset):
        if attribute in obj.attrs and obj.attrs[attribute] == value:
            return obj


def _v_item(key: Text, name: Text, obj: h5py.Dataset) -> h5py.Dataset:
    if key in name:
        return obj


def get_shape(h5file: h5py.File,
              item: H5Path) -> Tuple:
    res = None
    if isinstance(item, H5PathContains):
        res = h5file.visititems(partial(_v_item, item.path)).shape
    elif isinstance(item, H5PathOptionalItemValue):
        _item = h5file.visititems(partial(_v_item, item.path))
        res = _item.shape if _item else (1,)
    elif isinstance(item, H5PathWithAttribute):
        res = h5file.visititems(partial(_v_attrs,
                                        item.attribute, item.value)).shape
    return res


def get_item_at_index(h5file: h5py.File,
                      item: H5Path,
                      index: int) -> Union[float, ndarray]:
    res = None
    if isinstance(item, H5PathContains):
        res = h5file.visititems(partial(_v_item, item.path))[index]
    elif isinstance(item, H5PathOptionalItemValue):
        _item = h5file.visititems(partial(_v_item, item.path))
        res = _item.value if _item else item.default
    elif isinstance(item, H5PathWithAttribute):
        res = h5file.visititems(partial(_v_attrs,
                                        item.attribute, item.value))[index]
    return res
