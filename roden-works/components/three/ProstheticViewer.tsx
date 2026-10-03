'use client'

import { Component, Suspense, useEffect, useMemo, useRef, useState, type ReactNode } from 'react'
import { Canvas, useFrame } from '@react-three/fiber'
import {
  OrbitControls,
  Environment,
  Lightformer,
  ContactShadows,
  Html,
  useGLTF,
} from '@react-three/drei'
import * as THREE from 'three'
import type { OrbitControls as OrbitControlsImpl } from 'three-stdlib'

/* ─── Types ─────────────────────────────────────── */

interface Annotation {
  label: string
  detail: string
}

interface ModelOption {
  path: string
  label: string
}

interface ProstheticViewerProps {
  /** Array of .glb model paths in /public. */
  models?: ModelOption[]
  annotations?: Annotation[]
  className?: string
}

/* ─── Default models ─────────────────────────────── */

const DEFAULT_MODELS: ModelOption[] = [
  { path: '/models/va-dent-1.glb', label: 'Prototype 1' },
  { path: '/models/va-dent-2.glb', label: 'Prototype 2' },
]

/* ─── Default annotations ────────────────────────── */

// The VA models are assistive tools for placing and removing dentures, not dentures, so the old
// denture-anatomy callouts were removed. Pass annotations explicitly when they are confirmed.
const DEFAULT_ANNOTATIONS: Annotation[] = []

/* ─── WebGL availability ─────────────────────────── */

function hasWebGL() {
  try {
    const canvas = document.createElement('canvas')
    return Boolean(canvas.getContext('webgl2') || canvas.getContext('webgl'))
  } catch {
    return false
  }
}

/* ─── Error boundary: a 3D failure never takes the page down ─── */

class ViewerErrorBoundary extends Component<{ fallback: ReactNode; children: ReactNode }, { failed: boolean }> {
  state = { failed: false }
  static getDerivedStateFromError() {
    return { failed: true }
  }
  componentDidCatch(error: unknown) {
    console.warn('3D viewer unavailable:', error)
  }
  render() {
    return this.state.failed ? this.props.fallback : this.props.children
  }
}

function ViewerFallback() {
  return (
    <div className="absolute inset-0 flex flex-col items-center justify-center gap-3 p-8 text-center">
      <svg className="w-10 h-10 text-faint" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="1.2" aria-hidden="true">
        <path d="M12 3l8 4.5v9L12 21l-8-4.5v-9L12 3z" strokeLinejoin="round" />
        <path d="M12 12l8-4.5M12 12v9M12 12L4 7.5" strokeLinejoin="round" />
      </svg>
      <p className="text-sm text-titanium">Interactive 3D preview isn&apos;t available in this browser.</p>
      <p className="text-xs text-muted max-w-xs">The device components are described below.</p>
    </div>
  )
}

/* ─── GLB model: auto-centered, normalized, studio material ─── */

function GLBModel({ path, wireframe }: { path: string; wireframe: boolean }) {
  const { scene } = useGLTF(path)
  const groupRef = useRef<THREE.Group>(null)

  const normalizedScene = useMemo(() => {
    const clone = scene.clone(true)

    // Give each mesh its own material so toggles never leak between models,
    // and soften CAD-export materials (roughness 1.0 reads flat under studio light)
    clone.traverse((child) => {
      if (child instanceof THREE.Mesh) {
        const src = child.material as THREE.MeshStandardMaterial
        const mat = src.clone()
        if (mat instanceof THREE.MeshStandardMaterial && mat.roughness > 0.7) mat.roughness = 0.55
        mat.envMapIntensity = 1
        child.material = mat
        child.castShadow = true
      }
    })

    // Normalize so the largest dimension is 1.7 units (fits the 36° camera at any angle), then re-center.
    // Update child world matrices first: Box3.setFromObject only refreshes the root, so
    // models with scaled child nodes (va-dent-2) were measured too small and overflowed the frame.
    clone.updateMatrixWorld(true)
    const box = new THREE.Box3().setFromObject(clone, true)
    const size = new THREE.Vector3()
    box.getSize(size)
    const maxDim = Math.max(size.x, size.y, size.z)
    if (maxDim > 0) clone.scale.multiplyScalar(1.7 / maxDim)
    clone.updateMatrixWorld(true)
    const center = new THREE.Vector3()
    new THREE.Box3().setFromObject(clone, true).getCenter(center)
    clone.position.sub(center)

    return clone
  }, [scene])

  useEffect(() => {
    normalizedScene.traverse((child) => {
      if (child instanceof THREE.Mesh) {
        const mats = Array.isArray(child.material) ? child.material : [child.material]
        mats.forEach((m) => {
          if (m instanceof THREE.MeshStandardMaterial) m.wireframe = wireframe
        })
      }
    })
  }, [wireframe, normalizedScene])

  // Gentle idle float so the model never feels static
  useFrame((state) => {
    if (groupRef.current) groupRef.current.position.y = Math.sin(state.clock.elapsedTime * 0.8) * 0.03
  })

  return (
    <group ref={groupRef}>
      <primitive object={normalizedScene} />
    </group>
  )
}

/* ─── Loading indicator ──────────────────────────── */

function LoadingFallback() {
  return (
    <Html center>
      <div className="flex flex-col items-center gap-3">
        <div className="w-8 h-8 border-2 border-copper/30 border-t-copper rounded-full animate-spin" />
        <span className="font-mono text-xs text-titanium whitespace-nowrap">Loading model…</span>
      </div>
    </Html>
  )
}

/* ─── Studio lighting, generated locally (no network HDR) ─── */

function StudioEnvironment() {
  return (
    <Environment resolution={256} frames={1}>
      <group rotation={[-Math.PI / 3, 0, 1]}>
        <Lightformer form="circle" intensity={4} rotation-x={Math.PI / 2} position={[0, 5, -9]} scale={2} />
        <Lightformer form="circle" intensity={2} rotation-y={Math.PI / 2} position={[-5, 1, -1]} scale={2} />
        <Lightformer form="circle" intensity={2} rotation-y={Math.PI / 2} position={[-5, -1, -1]} scale={2} />
        <Lightformer form="circle" intensity={2} rotation-y={-Math.PI / 2} position={[10, 1, 0]} scale={8} />
        <Lightformer form="ring" color="#B87333" intensity={1.2} position={[0, -4, 6]} scale={4} />
      </group>
    </Environment>
  )
}

/* ─── Main viewer ────────────────────────────────── */

export default function ProstheticViewer({
  models = DEFAULT_MODELS,
  annotations = DEFAULT_ANNOTATIONS,
  className = 'aspect-square',
}: ProstheticViewerProps) {
  const [activeModel, setActiveModel] = useState(0)
  const [wireframe, setWireframe] = useState(false)
  const [activeAnnotation, setActiveAnnotation] = useState<number | null>(null)
  const [autoRotate, setAutoRotate] = useState(true)
  const [webgl, setWebgl] = useState<boolean | null>(null)
  const controlsRef = useRef<OrbitControlsImpl>(null)

  useEffect(() => setWebgl(hasWebGL()), [])

  return (
    <div className="flex flex-col">
      <div
        className={`relative rounded-xl overflow-hidden ${className}`}
        style={{
          background:
            'radial-gradient(ellipse at 50% 38%, rgba(138,155,168,0.14) 0%, rgba(21,28,31,0.9) 55%, #0E1518 100%)',
        }}
      >
        {/* Technical frame marks */}
        <div aria-hidden="true" className="pointer-events-none absolute inset-3 z-10">
          {['top-0 left-0 border-t border-l', 'top-0 right-0 border-t border-r', 'bottom-12 left-0 border-b border-l', 'bottom-12 right-0 border-b border-r'].map((pos) => (
            <span key={pos} className={`absolute w-4 h-4 border-white/20 ${pos}`} />
          ))}
          <span className="absolute top-1 left-6 font-mono text-[10px] tracking-widest uppercase text-muted">
            {models[activeModel].label} · Fusion 360 → GLB
          </span>
        </div>

        {webgl === false ? (
          <ViewerFallback />
        ) : webgl ? (
          <ViewerErrorBoundary fallback={<ViewerFallback />}>
            <Canvas
              camera={{ position: [2.3, 1.6, 2.5], fov: 36 }}
              dpr={[1, 2]}
              gl={{ antialias: true, alpha: true, toneMapping: THREE.ACESFilmicToneMapping }}
              style={{ background: 'transparent' }}
              aria-label={`Interactive 3D model: ${models[activeModel].label}. Drag to rotate, scroll to zoom.`}
            >
              <ambientLight intensity={0.25} />
              <directionalLight position={[4, 6, 5]} intensity={0.9} />
              <Suspense fallback={<LoadingFallback />}>
                <GLBModel key={models[activeModel].path} path={models[activeModel].path} wireframe={wireframe} />
                <ContactShadows position={[0, -0.9, 0]} opacity={0.6} scale={5} blur={2.2} far={2} />
                <StudioEnvironment />
              </Suspense>
              <OrbitControls
                ref={controlsRef}
                autoRotate={autoRotate}
                autoRotateSpeed={0.9}
                enablePan={false}
                enableDamping
                minDistance={1.6}
                maxDistance={8}
                maxPolarAngle={Math.PI * 0.8}
              />
            </Canvas>
          </ViewerErrorBoundary>
        ) : null}

        {/* Control bar */}
        <div className="absolute bottom-0 left-0 right-0 z-10 p-3 flex flex-wrap items-center gap-2 bg-gradient-to-t from-[#0E1518] via-[#0E1518]/70 to-transparent">
          {models.length > 1 &&
            models.map((model, i) => (
              <ControlButton
                key={model.path}
                active={activeModel === i}
                onClick={() => {
                  setActiveModel(i)
                  setActiveAnnotation(null)
                }}
                label={model.label}
              />
            ))}
          <span className="w-px h-4 bg-white/10 mx-1" aria-hidden="true" />
          <ControlButton active={wireframe} onClick={() => setWireframe(!wireframe)} label="Wireframe" />
          <ControlButton active={autoRotate} onClick={() => setAutoRotate(!autoRotate)} label="Rotate" />
          <ControlButton active={false} onClick={() => controlsRef.current?.reset()} label="Reset view" />
        </div>
      </div>

      {/* Annotation list — below the viewer */}
      {annotations.length > 0 && (
        <div className="grid grid-cols-1 sm:grid-cols-2 gap-2 mt-3">
          {annotations.map((ann, i) => (
            <button
              key={ann.label}
              type="button"
              aria-expanded={activeAnnotation === i}
              onClick={() => setActiveAnnotation(activeAnnotation === i ? null : i)}
              className={`text-left rounded-lg p-3 transition-colors duration-200 border ${
                activeAnnotation === i
                  ? 'bg-copper/10 border-copper/30'
                  : 'bg-white/[0.02] border-white/[0.06] hover:bg-white/[0.05]'
              }`}
            >
              <div className="flex items-center gap-2">
                <span
                  className={`w-5 h-5 rounded-full text-[10px] font-mono font-semibold flex items-center justify-center shrink-0 ${
                    activeAnnotation === i ? 'bg-copper text-slate-950' : 'bg-white/10 text-titanium'
                  }`}
                >
                  {i + 1}
                </span>
                <span className={`text-sm font-medium ${activeAnnotation === i ? 'text-white' : 'text-titanium'}`}>
                  {ann.label}
                </span>
              </div>
              {activeAnnotation === i && <p className="text-muted text-xs leading-relaxed mt-2 pl-7">{ann.detail}</p>}
            </button>
          ))}
        </div>
      )}
    </div>
  )
}

/* ─── Control button ─────────────────────────────── */

function ControlButton({ active, onClick, label }: { active: boolean; onClick: () => void; label: string }) {
  return (
    <button
      type="button"
      onClick={onClick}
      aria-pressed={active}
      className={`px-3 py-1.5 rounded-lg text-xs font-medium transition-colors duration-200 border ${
        active
          ? 'bg-copper/15 text-copper-light border-copper/30'
          : 'bg-white/[0.04] text-titanium border-white/10 hover:bg-white/10 hover:text-white'
      }`}
    >
      {label}
    </button>
  )
}
