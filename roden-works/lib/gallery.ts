import fs from 'node:fs'
import path from 'node:path'

export interface GalleryImage {
  src: string
  alt: string
  width: number
  height: number
}

const IMAGE_EXT = /\.(jpe?g|png|webp)$/i

/** Read pixel dimensions from a JPEG, PNG, or WebP header (no dependency). */
function readSize(buf: Buffer): { width: number; height: number } | null {
  // PNG: IHDR width/height at bytes 16–23
  if (buf.length > 24 && buf.readUInt32BE(0) === 0x89504e47) {
    return { width: buf.readUInt32BE(16), height: buf.readUInt32BE(20) }
  }
  // JPEG: walk markers to the first start-of-frame
  if (buf.length > 4 && buf[0] === 0xff && buf[1] === 0xd8) {
    let offset = 2
    while (offset + 9 < buf.length) {
      if (buf[offset] !== 0xff) return null
      const marker = buf[offset + 1]
      const isSOF = marker >= 0xc0 && marker <= 0xcf && marker !== 0xc4 && marker !== 0xc8 && marker !== 0xcc
      if (isSOF) return { height: buf.readUInt16BE(offset + 5), width: buf.readUInt16BE(offset + 7) }
      offset += 2 + buf.readUInt16BE(offset + 2)
    }
    return null
  }
  // WebP: RIFF container with VP8 / VP8L / VP8X chunk
  if (buf.length > 30 && buf.toString('ascii', 0, 4) === 'RIFF' && buf.toString('ascii', 8, 12) === 'WEBP') {
    const chunk = buf.toString('ascii', 12, 16)
    if (chunk === 'VP8 ') return { width: buf.readUInt16LE(26) & 0x3fff, height: buf.readUInt16LE(28) & 0x3fff }
    if (chunk === 'VP8L') {
      return {
        width: 1 + (((buf[22] & 0x3f) << 8) | buf[21]),
        height: 1 + (((buf[24] & 0x0f) << 10) | (buf[23] << 2) | ((buf[22] & 0xc0) >> 6)),
      }
    }
    if (chunk === 'VP8X') return { width: 1 + buf.readUIntLE(24, 3), height: 1 + buf.readUIntLE(27, 3) }
  }
  return null
}

/** "03-golden-hour_study.jpg" → "Golden hour study" */
function altFromFilename(file: string) {
  const words = file
    .replace(IMAGE_EXT, '')
    .replace(/^\d+[-_\s]*/, '')
    .replace(/[-_]+/g, ' ')
    .trim()
  return words ? words.charAt(0).toUpperCase() + words.slice(1) : 'Photograph'
}

/**
 * Lists the images in public/images/<folder>, sorted by filename, resolved at
 * build time. Returns [] when the folder is empty or missing, so pages can show
 * an intentional empty state instead of placeholder tiles.
 */
export function listGalleryImages(folder: string): GalleryImage[] {
  const dir = path.join(process.cwd(), 'public', 'images', folder)
  if (!fs.existsSync(dir)) return []
  return fs
    .readdirSync(dir)
    .filter((f) => IMAGE_EXT.test(f))
    .sort()
    .flatMap((file) => {
      const size = readSize(fs.readFileSync(path.join(dir, file)))
      if (!size || !size.width || !size.height) return []
      return [{ src: `/images/${folder}/${file}`, alt: altFromFilename(file), ...size }]
    })
}
