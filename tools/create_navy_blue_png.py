#!/usr/bin/env python

from PIL import Image, ImageCms


def create_srgb_profile() -> ImageCms.ImageCmsProfile:
    # Create a standard sRGB profile
    return ImageCms.createProfile("sRGB")


def embed_icc_profile(
    image: Image.Image, profile: ImageCms.ImageCmsProfile,
) -> Image.Image:
    # Embed the ICC profile
    icc_bytes = ImageCms.ImageCmsProfile(profile).tobytes()
    image.info["icc_profile"] = icc_bytes
    return image


# Set the dimensions of the image
width, height = 100, 100

# Create the image
img = Image.new("RGB", (width, height), color=(2, 42, 58))

# Create sRGB profile
srgb_profile = create_srgb_profile()

# Embed the sRGB profile
img_with_profile = embed_icc_profile(img, srgb_profile)

# Save the image as a PNG file with the embedded profile
img_with_profile.save(
    "Library/Application Support/desktoppr/navy_blue.png",
    icc_profile=img_with_profile.info.get("icc_profile"),
)
