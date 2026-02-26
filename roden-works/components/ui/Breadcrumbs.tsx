import Link from 'next/link'

interface BreadcrumbItem {
  label: string
  href?: string
}

interface BreadcrumbsProps {
  items: BreadcrumbItem[]
}

export default function Breadcrumbs({ items }: BreadcrumbsProps) {
  return (
    <nav aria-label="Breadcrumb" className="content-width pt-24 md:pt-28 pb-4">
      <ol className="flex items-center gap-2 text-sm font-mono">
        <li>
          <Link href="/" className="text-titanium hover:text-white transition-colors">
            Home
          </Link>
        </li>
        {items.map((item, i) => (
          <li key={i} className="flex items-center gap-2">
            <span className="text-titanium/40">/</span>
            {item.href ? (
              <Link href={item.href} className="text-titanium hover:text-white transition-colors">
                {item.label}
              </Link>
            ) : (
              <span className="text-white">{item.label}</span>
            )}
          </li>
        ))}
      </ol>
    </nav>
  )
}
