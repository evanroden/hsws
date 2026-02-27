import { SITE_CONFIG } from '@/lib/constants'

export function WebSiteJsonLd() {
  const data = {
    '@context': 'https://schema.org',
    '@type': 'WebSite',
    name: SITE_CONFIG.name,
    url: SITE_CONFIG.url,
    description: SITE_CONFIG.description,
  }
  return (
    <script
      type="application/ld+json"
      dangerouslySetInnerHTML={{ __html: JSON.stringify(data) }}
    />
  )
}

export function PersonJsonLd() {
  const data = {
    '@context': 'https://schema.org',
    '@type': 'Person',
    name: 'Evan Roden',
    jobTitle: 'Sustainability Engineer II / Asset Manager',
    worksFor: {
      '@type': 'Organization',
      name: 'ENFRA',
    },
    alumniOf: {
      '@type': 'CollegeOrUniversity',
      name: 'Tulane University',
    },
    url: SITE_CONFIG.url,
    sameAs: [
      SITE_CONFIG.linkedin,
      SITE_CONFIG.instagram,
      SITE_CONFIG.tiktok,
    ],
  }
  return (
    <script
      type="application/ld+json"
      dangerouslySetInnerHTML={{ __html: JSON.stringify(data) }}
    />
  )
}

export function BreadcrumbJsonLd({
  items,
}: {
  items: { name: string; href?: string }[]
}) {
  const data = {
    '@context': 'https://schema.org',
    '@type': 'BreadcrumbList',
    itemListElement: [
      { '@type': 'ListItem', position: 1, name: 'Home', item: SITE_CONFIG.url },
      ...items.map((item, i) => ({
        '@type': 'ListItem',
        position: i + 2,
        name: item.name,
        ...(item.href ? { item: `${SITE_CONFIG.url}${item.href}` } : {}),
      })),
    ],
  }
  return (
    <script
      type="application/ld+json"
      dangerouslySetInnerHTML={{ __html: JSON.stringify(data) }}
    />
  )
}
