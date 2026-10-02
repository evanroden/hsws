'use client'

import { useMemo, useState } from 'react'
import ChartFrame from '@/components/charts/ChartFrame'
import Legend from '@/components/charts/Legend'
import RangeSlider from '@/components/ui/RangeSlider'
import SegmentedControl from '@/components/ui/SegmentedControl'
import { chart } from '@/components/charts/tokens'
import WedgeProfile from './WedgeProfile'
import StatusGlyph, { sillGlyph } from './StatusGlyph'
import {
  FLOW_MAX,
  FLOW_MIN,
  FLOW_STEP,
  INTAKES,
  OVERTOP_FLOW,
  SILL_TEXT,
  SOURCES,
  STATUS_TEXT,
  THRESHOLD_FLOW,
  describeToe,
  intakeStatus,
  solveWedge,
  unobstructedToe,
  type SillCrest,
  type SourceId,
} from './wedgePhysics'

type Preset = 'spring' | 'threshold' | 'sept2023'

/* Presets. "Normal spring" uses the NPS average flow at New Orleans
 * (600,000 cfs); "Sept 2023" uses the ~148,000 cfs reported the week the
 * wedge first overtopped the −55 ft sill (NBC News), with the crest raised
 * to −30 ft as the Corps did from Sept 24. */
const PRESETS: Record<Preset, { label: string; flow: number; crest: SillCrest }> = {
  spring: { label: 'Normal spring', flow: 600_000, crest: 55 },
  threshold: { label: '300k threshold', flow: THRESHOLD_FLOW, crest: 55 },
  sept2023: { label: 'Sept 2023', flow: 148_000, crest: 30 },
}

const fmtFlow = (v: number) => `${v.toLocaleString('en-US')} cfs`

const NOTE_SOURCES: SourceId[] = ['dvids', 'fox8', 'noaa', 'wj0929', 'csm', 'nbc', 'wwno0919', 'nps']

export default function WedgeModel() {
  const [flow, setFlow] = useState(PRESETS.sept2023.flow)
  const [crest, setCrest] = useState<SillCrest>(PRESETS.sept2023.crest)

  const preset = (Object.keys(PRESETS) as Preset[]).find((k) => PRESETS[k].flow === flow && PRESETS[k].crest === crest)
  const state = useMemo(() => solveWedge(flow, crest), [flow, crest])

  const applyPreset = (p: Preset) => {
    setFlow(PRESETS[p].flow)
    setCrest(PRESETS[p].crest)
  }

  const threatened = INTAKES.map((lm) => ({ lm, status: intakeStatus(lm.rm, state) })).filter((i) => i.status !== 'clear')
  const toeText = state.toe < -1.2 ? 'Out of the river' : `≈ RM ${Math.max(0, Math.round(state.toe))}`

  const table = useMemo(() => {
    const flows = [600_000, 450_000, 300_000, 250_000, 200_000, 175_000, 150_000, 148_000, 130_000, 120_000, 100_000]
    return {
      caption: 'Illustrative modelled wedge toe by river flow, with the sill raised to −30 ft',
      columns: ['River flow', 'Toe without sill', 'Toe with −30 ft sill', 'Sill', 'Intakes affected'],
      rows: flows.map((f) => {
        const s = solveWedge(f, 30)
        const free = unobstructedToe(f)
        const hit = INTAKES.filter((lm) => ['salty', 'below'].includes(intakeStatus(lm.rm, s))).map((lm) => lm.name)
        return [
          fmtFlow(f),
          free <= 0 ? 'Out of river' : `RM ${free.toFixed(1)}`,
          s.toe <= 0 ? 'Out of river' : `RM ${s.toe.toFixed(1)}`,
          s.sill === 'none' ? 'Not built' : s.sill[0].toUpperCase() + s.sill.slice(1),
          hit.length ? hit.join(', ') : 'None',
        ]
      }),
    }
  }, [])

  return (
    <ChartFrame
      title="Lower Mississippi, Head of Passes to New Orleans"
      subtitle="Longitudinal profile: depth below the surface along the river. Denser Gulf water creeps upstream along the bed when the river runs low."
      actions={
        <SegmentedControl<Preset>
          label="Flow scenario"
          value={preset ?? ('' as Preset)}
          onChange={applyPreset}
          options={(Object.keys(PRESETS) as Preset[]).map((k) => ({ value: k, label: PRESETS[k].label }))}
        />
      }
      legend={
        <Legend
          items={[
            { label: 'Fresh river water', color: 'rgba(106,140,219,0.45)', shape: 'rect' },
            { label: 'Saltwater wedge', color: 'rgba(184,115,51,0.7)', shape: 'rect' },
            { label: 'Halocline (salt/fresh boundary)', color: chart.copper, shape: 'line' },
            { label: 'Channel bed & sill (schematic)', color: '#3A454D', shape: 'rect' },
          ]}
        />
      }
      note={
        <>
          <span className="font-medium text-titanium">Illustrative model, not a forecast.</span> Toe position is a
          straight-line fit from the Corps’ 300,000 cfs threshold to its projected worst-case 2023 toe (RM 103.7 at
          ~130,000 cfs); sill overtopping flows are calibrated to fall 2023. Bed profile is schematic. River miles and
          2023 facts:{' '}
          {NOTE_SOURCES.map((id, i) => (
            <span key={id}>
              <a href={SOURCES[id].url} target="_blank" rel="noopener noreferrer" className="underline decoration-white/20 underline-offset-2 hover:text-white">
                {SOURCES[id].label}
              </a>
              {i < NOTE_SOURCES.length - 1 ? '; ' : '.'}
            </span>
          ))}
        </>
      }
      table={table}
    >
      <WedgeProfile state={state} />

      <div className="mt-6 grid grid-cols-1 gap-6 border-t border-white/[0.06] pt-6 lg:grid-cols-[minmax(0,1fr)_minmax(0,1.15fr)] lg:gap-10">
        <div className="space-y-5">
          <RangeSlider
            label="River flow at New Orleans"
            value={flow}
            min={FLOW_MIN}
            max={FLOW_MAX}
            step={FLOW_STEP}
            onChange={setFlow}
            valueText={fmtFlow(flow)}
            accent={flow < THRESHOLD_FLOW ? chart.copper : chart.steel}
            ticks={[
              { value: 100_000, label: '100k' },
              { value: 300_000, label: '300k threshold' },
              { value: 600_000, label: '600k avg.' },
            ]}
          />
          <fieldset>
            <legend className="mb-2 text-sm text-titanium">Sill crest (when built)</legend>
            <SegmentedControl<string>
              label="Sill crest height"
              value={String(crest)}
              onChange={(v) => setCrest(Number(v) as SillCrest)}
              options={[
                { value: '55', label: 'July: −55 ft' },
                { value: '30', label: 'Raised: −30 ft' },
              ]}
            />
            <p className="mt-2 text-xs text-muted">
              Model: the −55 ft crest overtops below {fmtFlow(OVERTOP_FLOW[55])}; the −30 ft crest below{' '}
              {fmtFlow(OVERTOP_FLOW[30])}.
            </p>
          </fieldset>
        </div>

        <dl className="grid grid-cols-1 gap-x-6 gap-y-4 sm:grid-cols-2" aria-live="polite">
          <div>
            <dt className="font-mono text-[11px] uppercase tracking-widest text-muted">Wedge toe</dt>
            <dd className="mt-1 font-sans text-2xl font-semibold tracking-tight text-white">{toeText}</dd>
            <dd className="mt-0.5 text-xs text-titanium">{describeToe(state.toe)}</dd>
          </div>
          <div>
            <dt className="font-mono text-[11px] uppercase tracking-widest text-muted">Emergency sill · RM 64</dt>
            <dd className="mt-1.5 flex items-start gap-2 text-sm text-white">
              <StatusGlyph glyph={sillGlyph(state.sill)} className="mt-[3px]" />
              <span>{SILL_TEXT[state.sill]}</span>
            </dd>
          </div>
          <div className="sm:col-span-2">
            <dt className="font-mono text-[11px] uppercase tracking-widest text-muted">Drinking-water intakes</dt>
            <dd className="mt-2">
              {threatened.length === 0 ? (
                <p className="flex items-center gap-2 text-sm text-white">
                  <StatusGlyph glyph="clear" /> All {INTAKES.length} intakes on this reach are clear
                </p>
              ) : (
                <ul className="space-y-1.5">
                  {threatened.map(({ lm, status }) => (
                    <li key={lm.id} className="flex items-start gap-2 text-sm">
                      <StatusGlyph glyph={status} className="mt-[3px]" />
                      <span>
                        <span className="text-white">{lm.name}</span>{' '}
                        <span className="whitespace-nowrap tabular-nums text-muted">RM {lm.rm}</span>{' '}
                        <span className="whitespace-nowrap text-titanium">· {STATUS_TEXT[status].label}</span>
                      </span>
                    </li>
                  ))}
                  {INTAKES.length - threatened.length > 0 && (
                    <li className="flex items-center gap-2 text-sm text-muted">
                      <StatusGlyph glyph="clear" />
                      {INTAKES.length - threatened.length} further upstream clear
                      {threatened.some((t) => t.lm.id === 'carrollton') ? '' : ', incl. Carrollton'}
                    </li>
                  )}
                </ul>
              )}
            </dd>
          </div>
        </dl>
      </div>
    </ChartFrame>
  )
}
