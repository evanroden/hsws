import Link from 'next/link'
import { SITE_CONFIG } from '@/lib/constants'

interface GalleryPendingProps {
  title: string
  body: string
  /** Series or bodies of work the gallery will contain (from the page's own copy) */
  series?: string[]
  /** Subject line for the "request" email */
  requestSubject: string
  secondary?: { label: string; href: string }
}

/**
 * Shown in place of a gallery until images are added to public/images/<folder>.
 * Replaces rows of "coming soon" tiles with an intentional, useful panel.
 */
export default function GalleryPending({ title, body, series, requestSubject, secondary }: GalleryPendingProps) {
  return (
    <div className="rounded-2xl border border-white/[0.08] bg-surface p-6 md:p-10 grid grid-cols-1 md:grid-cols-5 gap-10">
      <div className="md:col-span-3">
        <span className="font-mono text-[11px] tracking-[0.18em] uppercase text-copper-light">Portfolio</span>
        <h2 className="mt-3 font-serif text-3xl md:text-4xl text-white">{title}</h2>
        <p className="mt-4 text-titanium leading-relaxed max-w-xl">{body}</p>
        <div className="mt-8 flex flex-wrap gap-3">
          <a
            href={`mailto:${SITE_CONFIG.email}?subject=${encodeURIComponent(requestSubject)}`}
            className="inline-flex items-center gap-2 rounded-lg bg-copper px-5 py-2.5 text-sm font-medium text-slate-950 hover:bg-copper-light transition-colors"
          >
            Request the portfolio
            <span aria-hidden="true">&rarr;</span>
          </a>
          {secondary && (
            <Link
              href={secondary.href}
              className="inline-flex items-center gap-2 rounded-lg border border-white/10 px-5 py-2.5 text-sm font-medium text-titanium hover:text-white hover:border-white/25 transition-colors"
            >
              {secondary.label}
            </Link>
          )}
        </div>
      </div>
      {series && series.length > 0 && (
        <div className="md:col-span-2 md:border-l md:border-white/[0.06] md:pl-10">
          <h3 className="font-mono text-[11px] tracking-[0.18em] uppercase text-muted">Series</h3>
          <ol className="mt-3 divide-y divide-white/[0.06]">
            {series.map((name, i) => (
              <li key={name} className="flex items-baseline gap-4 py-3">
                <span className="font-mono text-xs text-faint tabular-nums">{String(i + 1).padStart(2, '0')}</span>
                <span className="text-white">{name}</span>
              </li>
            ))}
          </ol>
        </div>
      )}
    </div>
  )
}
