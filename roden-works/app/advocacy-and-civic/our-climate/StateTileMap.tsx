'use client'

import { AnimatePresence, motion, useReducedMotion } from 'framer-motion'
import { useState } from 'react'
import ChartFrame from '@/components/charts/ChartFrame'
import ChartTooltip from '@/components/charts/ChartTooltip'
import { chart } from '@/components/charts/tokens'
import { useElementSize } from '@/components/charts/useElementSize'
import { TILE_COLS, TILE_GRID, TILE_ROWS } from './tileGrid'
import { Cite } from '@/components/ui/Sources'
import { OUR_CLIMATE_SOURCES as S } from './sources'

export interface Victory {
  state: string
  abbr: string
  title: string
  description: string
  sources?: string[]
}

/** De-emphasized tile: one step off the surface; labels in titanium clear 4.9:1 on it */
const TILE_FILL = chart.grid
const TILE_FILL_HOVER = '#2E393F'
const TILE_TEXT = '#8A9BA8'
const INK = '#0B1215'

export default function StateTileMap({ victories, animate }: { victories: Victory[]; animate: boolean }) {
  const [active, setActive] = useState(victories[0]?.abbr ?? '')
  const reduceMotion = useReducedMotion()

  return (
    <ChartFrame
      title="State climate wins around the 2019–20 fellowship"
      subtitle="Every state is drawn as an equal tile. Hover or select a highlighted state for what passed and when."
      legend={
        <ul className="flex flex-wrap items-center gap-x-5 gap-y-2">
          <li className="flex items-center gap-2 text-xs text-titanium">
            <span aria-hidden="true" className="inline-block h-3 w-3 rounded-[3px]" style={{ background: chart.verdigris }} />
            Climate win covered here
          </li>
          <li className="flex items-center gap-2 text-xs text-titanium">
            <span
              aria-hidden="true"
              className="inline-block h-3 w-3 rounded-[3px] border border-white/15"
              style={{ background: TILE_FILL }}
            />
            Other states
          </li>
        </ul>
      }
      note={
        <>
          Tile layout after NPR’s square tile grid map.<Cite sources={S} id="npr-tiles" />
        </>
      }
      table={{
        caption: 'State climate wins around the 2019–2020 Our Climate fellowship, with dates',
        columns: ['State', 'What passed'],
        rows: victories.map((v) => [v.state, v.title]),
      }}
    >
      <div className="grid grid-cols-1 gap-8 lg:grid-cols-[minmax(0,7fr)_minmax(0,5fr)] lg:items-center lg:gap-12">
        <TileMap victories={victories} active={active} onActivate={setActive} animate={animate} />

        <div>
          <p className="mb-3 font-mono text-[11px] uppercase tracking-widest text-muted">
            {victories.length} states covered
          </p>
          <ul className="border-t border-white/[0.08]">
            {victories.map((v) => {
              const on = v.abbr === active
              return (
                <li key={v.abbr} className="border-b border-white/[0.08]">
                  <button
                    type="button"
                    aria-expanded={on}
                    onClick={() => setActive(v.abbr)}
                    onFocus={() => setActive(v.abbr)}
                    onMouseEnter={() => setActive(v.abbr)}
                    className={`flex w-full items-start gap-4 rounded-lg px-2 py-4 text-left outline-none transition-colors focus-visible:ring-1 focus-visible:ring-white/40 ${
                      on ? 'bg-white/[0.03]' : 'hover:bg-white/[0.02]'
                    }`}
                  >
                    <span
                      aria-hidden="true"
                      className="flex h-9 w-9 shrink-0 items-center justify-center rounded-md text-xs font-semibold"
                      style={{ background: chart.verdigris, color: INK }}
                    >
                      {v.abbr}
                    </span>
                    <span className="min-w-0 pt-px">
                      <span className="block text-sm font-medium text-white">{v.state}</span>
                      <span className={`mt-0.5 block text-sm leading-snug ${on ? 'text-titanium' : 'text-muted'}`}>
                        {v.title}
                      </span>
                    </span>
                  </button>
                  <AnimatePresence initial={false}>
                    {on && (
                      <motion.div
                        key="detail"
                        initial={{ height: 0, opacity: 0 }}
                        animate={{ height: 'auto', opacity: 1 }}
                        exit={{ height: 0, opacity: 0 }}
                        transition={{ duration: reduceMotion ? 0 : 0.3, ease: [0.16, 1, 0.3, 1] }}
                        className="overflow-hidden"
                      >
                        <p className="pb-5 pl-2 sm:pl-[60px] pr-2 text-sm leading-relaxed text-titanium">
                          {v.description}
                          {v.sources && <Cite sources={S} id={v.sources} />}
                        </p>
                      </motion.div>
                    )}
                  </AnimatePresence>
                </li>
              )
            })}
          </ul>
        </div>
      </div>
    </ChartFrame>
  )
}

function TileMap({
  victories,
  active,
  onActivate,
  animate,
}: {
  victories: Victory[]
  active: string
  onActivate: (abbr: string) => void
  animate: boolean
}) {
  const { ref, width } = useElementSize<HTMLDivElement>()
  const reduceMotion = useReducedMotion()
  const [hover, setHover] = useState<string | null>(null)

  const compact = width > 0 && width < 480
  const gap = compact ? 3 : 4
  const pitch = (width + gap) / TILE_COLS
  const size = Math.max(0, pitch - gap)
  const height = Math.max(0, TILE_ROWS * pitch - gap)
  const radius = Math.max(3, Math.min(6, size * 0.12))
  const fontSize = Math.max(11, Math.min(14, size * 0.27))
  const drawn = animate || reduceMotion

  const byAbbr = (abbr: string) => victories.find((v) => v.abbr === abbr)
  const hovered = hover ? TILE_GRID.find((t) => t.abbr === hover) ?? null : null
  const hoveredVictory = hovered ? byAbbr(hovered.abbr) : undefined

  return (
    <div
      ref={ref}
      className="relative w-full"
      // Reserve the map's footprint before it is measured so the page doesn't jump
      style={width > 0 ? { height } : { aspectRatio: `${TILE_COLS} / ${TILE_ROWS}` }}
      onPointerLeave={() => setHover(null)}
    >
      {width > 0 && (
        <svg
          width={width}
          height={height}
          role="img"
          aria-label={`Tile grid map of the United States. ${victories
            .map((v) => v.state)
            .join(', ')} are highlighted as states with climate wins covered on this page; the other ${TILE_GRID.length - victories.length} states and DC are not.`}
          className="block"
        >
          {TILE_GRID.map((t) => {
            const victory = byAbbr(t.abbr)
            const isActive = victory && t.abbr === active
            const isHover = t.abbr === hover
            const x = t.col * pitch
            const y = t.row * pitch
            return (
              <motion.g
                key={t.abbr}
                initial={{ opacity: 0 }}
                animate={drawn ? { opacity: 1 } : {}}
                transition={{ duration: reduceMotion ? 0 : 0.4, delay: reduceMotion ? 0 : (t.col + t.row) * 0.025 }}
                onPointerEnter={() => {
                  setHover(t.abbr)
                  if (victory) onActivate(t.abbr)
                }}
                onPointerDown={() => {
                  setHover(t.abbr)
                  if (victory) onActivate(t.abbr)
                }}
                style={{ cursor: victory ? 'pointer' : 'default' }}
              >
                <rect
                  x={x}
                  y={y}
                  width={size}
                  height={size}
                  rx={radius}
                  fill={victory ? chart.verdigris : isHover ? TILE_FILL_HOVER : TILE_FILL}
                  className="transition-[fill] duration-150"
                />
                {isActive && (
                  <rect
                    x={x + 1.5}
                    y={y + 1.5}
                    width={size - 3}
                    height={size - 3}
                    rx={Math.max(1, radius - 1.5)}
                    fill="none"
                    stroke={chart.text.primary}
                    strokeWidth={2}
                  />
                )}
                <text
                  x={x + size / 2}
                  y={y + size / 2}
                  dy="0.35em"
                  textAnchor="middle"
                  fontSize={fontSize}
                  fontWeight={victory ? 700 : 500}
                  letterSpacing="0.02em"
                  fill={victory ? INK : isHover ? chart.text.primary : TILE_TEXT}
                  pointerEvents="none"
                >
                  {t.abbr}
                </text>
              </motion.g>
            )
          })}
        </svg>
      )}

      {hovered && (
        <ChartTooltip
          x={hovered.col * pitch + size / 2}
          y={hovered.row * pitch + size / 2}
          containerWidth={width}
          title={hovered.name}
        >
          {hoveredVictory ? (
            <div className="flex w-[210px] items-start gap-2 text-xs">
              <span aria-hidden="true" className="mt-[7px] inline-block h-0.5 w-3 shrink-0 rounded-full" style={{ background: chart.verdigris }} />
              <span className="leading-snug text-white">{hoveredVictory.title}</span>
            </div>
          ) : (
            <div className="text-xs text-muted">Not one of the states covered here</div>
          )}
        </ChartTooltip>
      )}
    </div>
  )
}
