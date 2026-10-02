/**
 * Footnoted sources for a page.
 *
 * Define one array per page, cite entries inline with <Cite>, and render the list
 * with <SourceList> at the bottom. Numbers come from array order, so the list and
 * the superscripts always agree.
 *
 *   const SOURCES = [{ id: 'rrh', title: '…', publisher: 'ENFRA', date: 'Jan 20, 2026', url: '…' }]
 *   <p>$143.8 million<Cite sources={SOURCES} id="rrh" /></p>
 *   <SourceList sources={SOURCES} />
 */

export interface Source {
  id: string
  title: string
  publisher: string
  date?: string
  url: string
  /** Which figures this source supports, shown after the link. */
  note?: string
}

export function sourceNumber(sources: Source[], id: string): number {
  const i = sources.findIndex((s) => s.id === id)
  if (i === -1) throw new Error(`Unknown source id "${id}"`)
  return i + 1
}

export function Cite({ sources, id }: { sources: Source[]; id: string | string[] }) {
  const ids = Array.isArray(id) ? id : [id]
  const nums = ids.map((x) => sourceNumber(sources, x))
  return (
    <sup className="ml-[0.1em] align-super text-[0.7em] font-mono font-medium leading-none not-italic">
      {nums.map((n, i) => (
        <span key={n}>
          {i > 0 && <span className="text-muted">,</span>}
          <a
            href={`#source-${n}`}
            aria-label={`Source ${n}`}
            className="text-copper-light no-underline hover:text-white focus-visible:text-white transition-colors"
          >
            {n}
          </a>
        </span>
      ))}
    </sup>
  )
}

export function SourceList({ sources, title = 'Sources' }: { sources: Source[]; title?: string }) {
  if (sources.length === 0) return null
  return (
    <section aria-labelledby="sources-heading" className="bg-slate-950 border-t border-white/[0.06]">
      <div className="content-width py-14 md:py-16">
        <h2 id="sources-heading" className="font-mono text-xs tracking-widest uppercase text-copper">
          {title}
        </h2>
        <ol className="mt-6 space-y-3 max-w-3xl text-sm leading-relaxed">
          {sources.map((s, i) => (
            <li key={s.id} id={`source-${i + 1}`} className="scroll-mt-28 flex gap-3 text-titanium">
              <span className="w-6 shrink-0 text-right font-mono text-xs text-copper-light pt-[0.2em] tabular-nums">
                {i + 1}
              </span>
              <span>
                <a
                  href={s.url}
                  target="_blank"
                  rel="noopener noreferrer"
                  className="text-white underline decoration-white/25 underline-offset-[3px] hover:decoration-copper-light transition-colors"
                >
                  {s.title}
                </a>
                <span className="text-muted">
                  {' · '}
                  {s.publisher}
                  {s.date ? `, ${s.date}` : ''}
                </span>
                {s.note && <span className="block text-xs text-muted mt-0.5">{s.note}</span>}
              </span>
            </li>
          ))}
        </ol>
      </div>
    </section>
  )
}
