#!/usr/bin/env python

from PIL import Image, ImageCms

img = Image.new("RGB", (100, 100), color=(2, 42, 58))

icc_bytes = ImageCms.ImageCmsProfile(ImageCms.createProfile("sRGB")).tobytes()  # pyright: ignore[reportUnknownMemberType,reportUnknownArgumentType]

img.save(
    "Library/Application Support/desktoppr/navy_blue.png",
    icc_profile=icc_bytes,
)
