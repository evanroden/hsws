import { MetadataRoute } from 'next'

export default function sitemap(): MetadataRoute.Sitemap {
  const baseUrl = 'https://roden.works'

  const pages = [
    '',
    '/about',
    '/engineering-and-sustainability',
    '/engineering-and-sustainability/enfra',
    '/engineering-and-sustainability/convergint',
    '/engineering-and-sustainability/odoo',
    '/engineering-and-sustainability/research',
    '/engineering-and-sustainability/research/va-prosthetics',
    '/engineering-and-sustainability/research/haps',
    '/engineering-and-sustainability/research/swis',
    '/engineering-and-sustainability/research/wimley-lab',
    '/advocacy-and-civic',
    '/advocacy-and-civic/ycod',
    '/studio',
    '/studio/cinematography',
    '/studio/glass-art',
    '/studio/photography',
    '/studio/modeling',
    '/about/ted',
  ]

  return pages.map((path) => ({
    url: `${baseUrl}${path}`,
    lastModified: new Date(),
    changeFrequency: path === '' ? 'weekly' : 'monthly',
    priority: path === '' ? 1 : path.split('/').length <= 2 ? 0.8 : 0.6,
  }))
}
