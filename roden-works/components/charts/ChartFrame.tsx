'use client'

import { useId, useState, type ReactNode } from 'react'

export interface ChartTable {
  caption?: string
  columns: string[]
  rows: (string | number)[][]
}

interface ChartFrameProps {
  /** Sentence-case chart title (what is plotted) */
  title: string
  subtitle?: ReactNode
  /** Controls that change the view, e.g. a SegmentedControl */
  actions?: ReactNode
  legend?: ReactNode
  /** Accessible twin of the chart — every value reachable without hover */
  table?: ChartTable
  /** Source / methodology line */
  note?: ReactNode
  children: ReactNode
  className?: string
}

/**
 * The container every chart mounts in: title, optional view controls,
 * a table-view toggle (the accessibility twin), legend and source note.
 */
export default function ChartFrame({
  title,
  subtitle,
  actions,
  legend,
  table,
  note,
  children,
  className = '',
}: ChartFrameProps) {
  const [showTable, setShowTable] = useState(false)
  const titleId = useId()

  return (
    <figure
      aria-labelledby={titleId}
      className={`rounded-2xl border border-white/[0.08] bg-surface p-5 md:p-8 ${className}`}
    >
      <div className="flex flex-col gap-4 md:flex-row md:items-start md:justify-between mb-6">
        <div className="min-w-0">
          <h3 id={titleId} className="font-sans text-base md:text-lg font-medium text-white">
            {title}
          </h3>
          {subtitle && <p className="mt-1 text-sm text-muted max-w-2xl">{subtitle}</p>}
        </div>
        <div className="flex flex-wrap items-center gap-2 shrink-0">
          {actions}
          {table && (
            <button
              type="button"
              onClick={() => setShowTable((v) => !v)}
              aria-pressed={showTable}
              className="inline-flex items-center gap-1.5 rounded-lg border border-white/[0.08] px-3 py-1.5 text-xs font-medium text-muted hover:text-white hover:border-white/20 transition-colors"
            >
              <svg className="w-3.5 h-3.5" viewBox="0 0 16 16" fill="none" stroke="currentColor" strokeWidth="1.5" aria-hidden="true">
                {showTable ? (
                  <path d="M2 12l4-4 3 3 5-6" strokeLinecap="round" strokeLinejoin="round" />
                ) : (
                  <>
                    <rect x="2" y="3" width="12" height="10" rx="1.5" />
                    <path d="M2 7h12M6 7v6" />
                  </>
                )}
              </svg>
              {showTable ? 'Chart view' : 'Table view'}
            </button>
          )}
        </div>
      </div>

      {showTable && table ? (
        <div className="overflow-x-auto">
          <table className="w-full text-sm">
            {table.caption && <caption className="sr-only">{table.caption}</caption>}
            <thead>
              <tr className="border-b border-white/10">
                {table.columns.map((c) => (
                  <th key={c} scope="col" className="py-2 pr-6 text-left font-medium text-muted whitespace-nowrap">
                    {c}
                  </th>
                ))}
              </tr>
            </thead>
            <tbody>
              {table.rows.map((row, i) => (
                <tr key={i} className="border-b border-white/[0.05]">
                  {row.map((cell, j) => (
                    <td
                      key={j}
                      className={`py-2 pr-6 whitespace-nowrap ${j === 0 ? 'text-titanium' : 'text-white tabular-nums'}`}
                    >
                      {cell}
                    </td>
                  ))}
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      ) : (
        children
      )}

      {(legend || note) && (
        <figcaption className="mt-6 pt-5 border-t border-white/[0.06] flex flex-col gap-3 md:flex-row md:items-center md:justify-between">
          <div>{legend}</div>
          {note && <p className="text-xs text-muted md:text-right max-w-xl">{note}</p>}
        </figcaption>
      )}
    </figure>
  )
}
