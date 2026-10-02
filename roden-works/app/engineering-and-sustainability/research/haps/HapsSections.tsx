'use client'

import { motion } from 'framer-motion'
import { useState } from 'react'
import { useInView } from '@/lib/hooks'
import HomeCutaway from './HomeCutaway'
import {
  MONITORS,
  POLLUTANTS,
  POLLUTANT_ORDER,
  WHO_AQG,
  monitorFor,
  sourcesFor,
  type Filter,
  type PollutantId,
} from './data'
import { INSTRUMENT_ICONS, PollutantMark } from './icons'
import { Cite } from '@/components/ui/Sources'
import { HAPS_SOURCES as S } from './sources'

/* ─── Pollutant profiles: interactive home cutaway + pollutant cards ────── */

export function PollutantProfiles() {
  const { ref, isInView } = useInView(0.05)
  const [filter, setFilter] = useState<Filter>('all')

  return (
    <section className="section-padding bg-gradient-to-b from-slate-950 to-copper/5" ref={ref}>
      <div className="content-width">
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.6 }}
          className="mb-10 md:mb-12 max-w-3xl"
        >
          <span className="font-mono text-xs tracking-widest uppercase text-copper">Pollutant Profiles</span>
          <h2 className="font-serif text-heading text-white mt-3">Pollution sources in the home.</h2>
          <p className="mt-4 text-titanium leading-relaxed">
            Gas appliances, cooking, candles, tobacco and traffic outside the door each add to the air
            inside a home. Filter by pollutant to see where it comes from and which instrument measured it.
          </p>
        </motion.div>

        <motion.div
          initial={{ opacity: 0, y: 24 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.7, delay: 0.1 }}
        >
          <HomeCutaway filter={filter} onFilterChange={setFilter} />
        </motion.div>

        <div className="mt-6 grid grid-cols-1 gap-4 lg:grid-cols-3">
          {POLLUTANT_ORDER.map((id, i) => (
            <motion.div
              key={id}
              initial={{ opacity: 0, y: 20 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.5, delay: 0.2 + i * 0.1 }}
            >
              <PollutantCard
                id={id}
                active={filter === id}
                dimmed={filter !== 'all' && filter !== id}
                onSelect={() => setFilter(filter === id ? 'all' : id)}
              />
            </motion.div>
          ))}
        </div>

        <p className="mt-5 text-xs text-muted max-w-3xl">
          WHO guideline values: WHO global air quality guidelines (2021), Table 0.1. 24-hour values are the
          99th percentile, i.e. 3–4 exceedance days per year; the guidelines apply to indoor as well as
          outdoor air. For black carbon, WHO found the evidence insufficient to set a guideline level and
          issues good-practice statements instead.<Cite sources={S} id="who-aqg" />
        </p>
      </div>
    </section>
  )
}

function PollutantCard({
  id,
  active,
  dimmed,
  onSelect,
}: {
  id: PollutantId
  active: boolean
  dimmed: boolean
  onSelect: () => void
}) {
  const p = POLLUTANTS[id]
  const who = WHO_AQG[id]
  const mon = monitorFor(id)
  const srcs = sourcesFor(id)

  return (
    <article
      className={`flex h-full flex-col rounded-2xl border bg-surface p-5 md:p-6 transition-[opacity,border-color] duration-300 ${
        active ? 'border-white/25' : 'border-white/[0.08]'
      } ${dimmed ? 'opacity-50' : 'opacity-100'}`}
    >
      <div className="flex items-center justify-between gap-3">
        <h3 className="flex items-center gap-2.5 font-sans text-lg font-medium text-white">
          <PollutantMark id={id} size={12} />
          {p.label}
        </h3>
        <button
          type="button"
          onClick={onSelect}
          aria-pressed={active}
          className="rounded-md border border-white/[0.08] px-2.5 py-1 text-xs font-medium text-muted hover:border-white/20 hover:text-white transition-colors"
        >
          {active ? 'Show all' : 'Show on diagram'}
        </button>
      </div>

      <p className="mt-3 text-sm leading-relaxed text-titanium">
        {p.description}
        <Cite sources={S} id={p.cites} />
      </p>

      <dl className="mt-5 space-y-3 border-t border-white/[0.06] pt-4 text-sm">
        <div className="flex gap-3">
          <dt className="w-24 shrink-0 font-mono text-[11px] uppercase tracking-wider text-muted pt-0.5">
            Sources
          </dt>
          <dd className="text-white/90">{srcs.map((s) => s.name).join(', ')}</dd>
        </div>
        <div className="flex gap-3">
          <dt className="w-24 shrink-0 font-mono text-[11px] uppercase tracking-wider text-muted pt-0.5">
            Measured by
          </dt>
          <dd className="text-white/90">{mon?.name}</dd>
        </div>
      </dl>

      <div className="mt-auto pt-5">
        <p className="font-mono text-[11px] uppercase tracking-wider text-muted">
          WHO guideline (2021)
          <Cite sources={S} id="who-aqg" />
        </p>
        {who ? (
          <div className="mt-2 grid grid-cols-2 gap-3">
            <GuidelineValue value={who.annual} label="annual mean" />
            <GuidelineValue value={who.day} label="24-hour mean" />
          </div>
        ) : (
          <p className="mt-2 text-sm text-titanium">
            No guideline value. Evidence was judged insufficient to set one; WHO recommends measuring and
            reducing it.
          </p>
        )}
      </div>
    </article>
  )
}

function GuidelineValue({ value, label }: { value: number; label: string }) {
  return (
    <div className="rounded-lg bg-white/[0.03] px-3 py-2.5">
      <p className="font-sans text-white">
        <span className="text-2xl font-semibold tracking-tight">{value}</span>
        <span className="ml-1 text-xs text-muted">µg/m³</span>
      </p>
      <p className="text-xs text-muted">{label}</p>
    </div>
  )
}

/* ─── The evidence ─────────────────────────────────────────────────────── */

// Lewington S, et al. (Prospective Studies Collaboration). Age-specific relevance of
// usual blood pressure to vascular mortality. Lancet 2002;360:1903–13.
// doi:10.1016/S0140-6736(02)11911-8 — the ≈7% IHD / ≈10% stroke mortality per
// 2 mmHg usual SBP relationship already cited on this page.
const POPULATION_EFFECTS = [
  { value: '≈10%', label: 'higher stroke mortality', share: 10 },
  { value: '≈7%', label: 'higher ischemic heart disease mortality', share: 7 },
]

export function Evidence() {
  const { ref, isInView } = useInView(0.1)
  return (
    <section className="section-padding bg-slate-950" ref={ref}>
      <div className="content-width">
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.6 }}
          className="mb-10 md:mb-12"
        >
          <span className="font-mono text-xs tracking-widest uppercase text-copper">Published Finding</span>
          <h2 className="font-serif text-heading text-white mt-3">The evidence.</h2>
        </motion.div>

        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.6, delay: 0.15 }}
          className="grid grid-cols-1 overflow-hidden rounded-2xl border border-white/[0.08] bg-surface lg:grid-cols-[1.1fr_1fr]"
        >
          {/* Published finding: Rabito FA, et al. "The association between
              short-term residential black carbon concentration on blood
              pressure in a general population sample" (2021), PMC7985991 —
              a 1 µg/m³ increase in black carbon was associated with a 7.55 mmHg
              rise in systolic BP (P = .02).
              https://pmc.ncbi.nlm.nih.gov/articles/PMC7985991/ */}
          <div className="p-6 md:p-10">
            <p className="font-mono text-[11px] uppercase tracking-wider text-muted">
              Per 1 µg/m³ of residential black carbon
            </p>
            <p className="mt-3 font-sans font-semibold tracking-tight text-white leading-none text-[56px] md:text-[80px]">
              +7.55
              <span className="ml-2 align-baseline text-2xl md:text-3xl font-medium text-titanium">mmHg</span>
            </p>
            <p className="mt-4 flex items-start gap-2 text-lg text-white">
              <span aria-hidden="true" className="mt-[0.8em] inline-block h-0.5 w-5 shrink-0 rounded-full bg-copper" />
              Systolic blood pressure increase
            </p>
            <p className="mt-3 max-w-lg text-sm leading-relaxed text-titanium">
              Each 1 µg/m³ increase in residential black carbon was associated with a 7.55 mmHg rise in
              systolic blood pressure.<Cite sources={S} id="rabito" /> Across a whole population, a shift like this matters, especially in
              communities that already have high rates of cardiovascular disease.
            </p>
          </div>

          <div className="border-t border-white/[0.06] bg-white/[0.015] p-6 md:p-10 lg:border-l lg:border-t-0">
            <p className="font-mono text-[11px] uppercase tracking-wider text-muted">
              What 2 mmHg means at population scale
            </p>
            <ul className="mt-5 space-y-6">
              {POPULATION_EFFECTS.map((e, i) => (
                <li key={e.label}>
                  <div className="flex items-baseline gap-3">
                    <span className="font-sans text-4xl md:text-5xl font-semibold tracking-tight text-white">
                      {e.value}
                    </span>
                    <span className="text-sm text-titanium">{e.label}</span>
                  </div>
                  {/* Relative-risk bar: the scale runs 0–10% so the two effects compare directly */}
                  <div className="mt-3">
                    <div className="h-2 rounded-full bg-white/[0.06]" aria-hidden="true">
                      <motion.div
                        className="h-full rounded-full bg-copper"
                        initial={{ width: 0 }}
                        animate={isInView ? { width: `${e.share * 10}%` } : {}}
                        transition={{ duration: 0.9, delay: 0.4 + i * 0.15, ease: [0.16, 1, 0.3, 1] }}
                      />
                    </div>
                  </div>
                </li>
              ))}
            </ul>
            <p className="mt-6 text-xs leading-relaxed text-muted">
              A 2 mmHg population-level increase in systolic BP is associated with a 7% increase in
              ischemic heart disease mortality and a 10% increase in stroke mortality (Lewington et al.,
              Lancet 2002; meta-analysis of one million adults in 61 prospective studies).<Cite sources={S} id="lewington" /> These are
              population associations, not deaths estimated from this study.
            </p>
          </div>
        </motion.div>
      </div>
    </section>
  )
}

/* ─── Measurement instruments ──────────────────────────────────────────── */

export function Instruments() {
  const { ref, isInView } = useInView(0.1)
  return (
    <section className="section-padding bg-gradient-to-b from-slate-950 to-forest/5" ref={ref}>
      <div className="content-width">
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.6 }}
          className="mb-10 md:mb-12"
        >
          <span className="font-mono text-xs tracking-widest uppercase text-copper">Methodology</span>
          <h2 className="font-serif text-heading text-white mt-3">Measurement instruments.</h2>
        </motion.div>

        <div className="grid grid-cols-1 gap-4 sm:grid-cols-2 lg:grid-cols-4">
          {MONITORS.map((m, i) => {
            const Icon = INSTRUMENT_ICONS[m.id as keyof typeof INSTRUMENT_ICONS]
            return (
              <motion.div
                key={m.id}
                initial={{ opacity: 0, y: 20 }}
                animate={isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.5, delay: i * 0.08 }}
                className="flex flex-col rounded-2xl border border-white/[0.08] bg-surface p-5"
              >
                <div className="flex items-center justify-between">
                  <span className="flex h-11 w-11 items-center justify-center rounded-xl border border-white/[0.08] bg-white/[0.03] text-titanium">
                    <Icon size={24} />
                  </span>
                  <span className="font-mono text-[11px] uppercase tracking-wider text-muted">
                    {m.placement}
                  </span>
                </div>
                <h3 className="mt-5 font-sans text-base font-medium text-white">{m.name}</h3>
                <p className="mt-1 flex items-center gap-2 text-sm text-titanium">
                  {m.pollutant && <PollutantMark id={m.pollutant} size={9} />}
                  {m.measures}
                </p>
              </motion.div>
            )
          })}
        </div>
      </div>
    </section>
  )
}
